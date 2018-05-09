{-# LANGUAGE TemplateHaskell #-}
import Common.Lens hiding ((<.>))

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Zip

import Data.Bits
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid

import Distribution.InstalledPackageInfo
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
import Distribution.Simple.PackageIndex
import Distribution.Text
import Distribution.Types.ComponentName
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.UnitId

import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Files

import Text.Read

data Info =
  Info
  {
    newRoot :: FilePath,
    ghcVersion :: String
  }

type InfoM = ReaderT Info IO

makeLenses ''InstalledPackageInfo
makeLenses ''Info

main :: IO ()
main = do
  newRoot <- getCurrentDirectory
  projectRoot <- getProjectRoot newRoot
  createDirectoryIfMissing True (libSubdir newRoot)
  createDirectoryIfMissing True (packageConfDSubdir newRoot)
  createDirectoryIfMissing True (moduleSubdir newRoot)
  (ghcVersion, packageInfo, packageIndex) <- getBuildInfo projectRoot
  flip runReaderT Info{..} $
    forM_ (packageInfo : allPackages packageIndex) copyPackage

copyPackage :: InstalledPackageInfo -> InfoM ()
copyPackage packageInfo@InstalledPackageInfo{installedUnitId} = do
  Info{..} <- ask
  let 
    newSpecPath = 
      packageConfDSubdir newRoot </> display installedUnitId <.> "conf"
  writePackageSpec packageInfo newSpecPath
  copyLibraryDirs packageInfo
  copyDynLibs packageInfo
 
writePackageSpec :: InstalledPackageInfo -> FilePath -> InfoM ()
writePackageSpec packageInfo newSpecPath = do
  Info{..} <- ask
  let newPackageInfo = minimalize $ moveRoot packageInfo
  liftIO $ writeFile newSpecPath $ showInstalledPackageInfo newPackageInfo

copyLibraryDirs :: InstalledPackageInfo -> InfoM ()
copyLibraryDirs InstalledPackageInfo{libraryDirs, importDirs} = do
  Info{..} <- ask
  liftIO $ forM_ (libraryDirs `union` importDirs) $ \libDir -> do
    let newDir = replaceDirectory libDir (moduleSubdir newRoot)
    alreadyCopied <- doesDirectoryExist newDir
    unless alreadyCopied $ fastCopyDir newDir libDir

copyDynLibs :: InstalledPackageInfo -> InfoM ()
copyDynLibs InstalledPackageInfo{libraryDirs,libraryDynDirs,hsLibraries} = do
  Info{..} <- ask
  liftIO $ forM_ nonCLibraries $ \libName -> do
    let prefixedLib = "lib" ++ libName
        addSO = (<.> "so")
        libBaseName = addSO $ prefixedLib ++ "-" ++ ghcVersion
        altLibBaseName = addSO prefixedLib 
    oldLibPathM <- altFileExistsInPath libBaseName altLibBaseName &
      sequencePath (libraryDirs `union` libraryDynDirs)
    case oldLibPathM of
      Nothing -> error $ "Dynamic library not found for: " ++ libName
      Just oldLibPath -> do
        let newLibPath = replaceDirectory oldLibPath (libSubdir newRoot)
        alreadyCopied <- doesFileExist newLibPath 
        unless alreadyCopied $ createLink oldLibPath newLibPath
  where
    altFileExistsInPath fileName1 fileName2 path = 
      getFirst <$> liftM2 ((<>) `on` First) 
        (maybeFileExists $ path </> fileName1)
        (maybeFileExists $ path </> fileName2)
    maybeFileExists path = 
      bool Nothing (Just path) <$> doesFileExist path
    nonCLibraries = nub $ removeC <$> hsLibraries
    removeC x = fromMaybe x (stripPrefix "C" x)

fastCopyDir :: FilePath -> FilePath -> IO ()
fastCopyDir newDir oldDir = do
  createDirectory newDir
  allFiles <- listDirectory oldDir
  forM_ allFiles $ \file -> do
    let oldFilePath = oldDir </> file
        newFilePath = newDir </> file
    isDirectory <- doesDirectoryExist oldFilePath
    if isDirectory
    then fastCopyDir newFilePath oldFilePath
    else when (takeExtension file == ".dyn_hi") $ 
      createLink oldFilePath newFilePath

getBuildInfo :: 
  FilePath -> IO (String, InstalledPackageInfo, InstalledPackageIndex)
getBuildInfo projectRoot = do
  distDir <- getEnv "HASKELL_DIST_DIR"
  lbi@LocalBuildInfo{compiler, installedPkgs} <- 
    getPersistBuildConfig (projectRoot </> distDir)
  packageInfo <- getLibComponent lbi
  let CompilerId flavor version = compilerId compiler 
      ghcVersion = display flavor ++ display version
  return (ghcVersion, packageInfo, installedPkgs)

getLibComponent :: LocalBuildInfo -> IO InstalledPackageInfo
getLibComponent LocalBuildInfo{componentNameMap, withPackageDB} = do
  case Map.lookup CLibName componentNameMap of
    Nothing -> error $ "Local project does not define a library"
    Just [cLocalBuildInfo] -> do
      let unitID = componentUnitId cLocalBuildInfo
          pkgdb = pkgdbPath $ registrationPackageDB withPackageDB
          confFilePath = pkgdb </> display unitID <.> "conf"
      parseResult <- parseInstalledPackageInfo <$> readFile confFilePath
      case parseResult of
        ParseFailed err -> error $ 
          "Couldn't parse package install configuration file: " ++ confFilePath
        ParseOk _ packageInfo -> return packageInfo
    _ -> error $ "Local project (somehow) defines multiple libraries"

  where
    pkgdbPath (SpecificPackageDB path) = path
    pkgdbPath _ = error "Registration db isn't the project's"

getProjectRoot :: FilePath -> IO FilePath
getProjectRoot [pathSeparator] = error "No stack project found"
getProjectRoot dir = do
  atRoot <- doesFileExist $ dir </> "stack" <.> "yaml"
  if atRoot
  then return dir
  else getProjectRoot (takeDirectory dir)

sequencePath :: (Monad m) => [FilePath] -> (FilePath -> m (Maybe a)) -> m (Maybe a)
sequencePath paths f = go paths Nothing where
  go [] Nothing = return Nothing
  go _ (Just x) = return $ Just x
  go (path : rest) Nothing = f path >>= go rest

moveRoot :: InstalledPackageInfo -> InstalledPackageInfo
moveRoot pkgInfo = pkgInfo
  & _importDirs %~ map chroot
  & _libraryDirs %~ map chroot
  & _includeDirs %~ map chroot
  & _libraryDynDirs .~ [libDir]
  & _dataDir .~ root
  where 
    chroot = flip replaceDirectory moduleDir
    moduleDir = moduleSubdir root
    libDir = libSubdir root
    root = [pathSeparator]

minimalize :: InstalledPackageInfo -> InstalledPackageInfo
minimalize pkgInfo = pkgInfo
  & _haddockInterfaces .~ []
  & _haddockHTMLs .~ []

libSubdir :: FilePath -> FilePath
libSubdir = (</> "lib")

packageConfDSubdir :: FilePath -> FilePath
packageConfDSubdir p = p </> "etc" </> "fae" </> "package.conf.d"

moduleSubdir :: FilePath -> FilePath
moduleSubdir = (</> "include")
