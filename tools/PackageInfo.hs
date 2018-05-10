{- 
- Copied wholesale from
- https://github.com/ghc/ghc/blob/master/utils/ghc-pkg/Main.hs#L1226-L1308
-}
module PackageInfo where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Version as Version

import Distribution.Backpack
import Distribution.InstalledPackageInfo
import qualified Distribution.ModuleName as ModuleName
import Distribution.ModuleName (ModuleName)
import Distribution.Package hiding (installedUnitId)
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Types.UnqualComponentName
import Distribution.Version

import qualified GHC.PackageDb as GhcPkg
import GHC.PackageDb (BinaryStringRep(..))

type PackageCacheFormat = GhcPkg.InstalledPackageInfo
  ComponentId
  PackageIdentifier
  PackageName
  UnitId
  OpenUnitId
  ModuleName
  OpenModule

convertPackageInfoToCacheFormat :: InstalledPackageInfo -> PackageCacheFormat
convertPackageInfoToCacheFormat pkg =
    GhcPkg.InstalledPackageInfo {
      GhcPkg.unitId             = installedUnitId pkg,
      GhcPkg.componentId        = installedComponentId pkg,
      GhcPkg.instantiatedWith   = instantiatedWith pkg,
      GhcPkg.sourcePackageId    = sourcePackageId pkg,
      GhcPkg.packageName        = packageName pkg,
      GhcPkg.packageVersion     = Version.Version (versionNumbers (packageVersion pkg)) [],
      GhcPkg.sourceLibName      =
        fmap (mkPackageName . unUnqualComponentName) (sourceLibName pkg),
      GhcPkg.depends            = depends pkg,
      GhcPkg.abiDepends         = map (\(AbiDependency k v) -> (k,unAbiHash v)) (abiDepends pkg),
      GhcPkg.abiHash            = unAbiHash (abiHash pkg),
      GhcPkg.importDirs         = importDirs pkg,
      GhcPkg.hsLibraries        = hsLibraries pkg,
      GhcPkg.extraLibraries     = extraLibraries pkg,
      GhcPkg.extraGHCiLibraries = extraGHCiLibraries pkg,
      GhcPkg.libraryDirs        = libraryDirs pkg,
      GhcPkg.libraryDynDirs     = libraryDynDirs pkg,
      GhcPkg.frameworks         = frameworks pkg,
      GhcPkg.frameworkDirs      = frameworkDirs pkg,
      GhcPkg.ldOptions          = ldOptions pkg,
      GhcPkg.ccOptions          = ccOptions pkg,
      GhcPkg.includes           = includes pkg,
      GhcPkg.includeDirs        = includeDirs pkg,
      GhcPkg.haddockInterfaces  = haddockInterfaces pkg,
      GhcPkg.haddockHTMLs       = haddockHTMLs pkg,
      GhcPkg.exposedModules     = map convertExposed (exposedModules pkg),
      GhcPkg.hiddenModules      = hiddenModules pkg,
      GhcPkg.indefinite         = indefinite pkg,
      GhcPkg.exposed            = exposed pkg,
      GhcPkg.trusted            = trusted pkg
      }
    where
      convertExposed (ExposedModule n reexport) = (n, reexport)

instance GhcPkg.BinaryStringRep ComponentId where
    fromStringRep = mkComponentId . fromStringRep
    toStringRep   = toStringRep . display

instance GhcPkg.BinaryStringRep PackageName where
    fromStringRep = mkPackageName . fromStringRep
    toStringRep   = toStringRep . display

instance GhcPkg.BinaryStringRep PackageIdentifier where
    fromStringRep = fromMaybe (error "BinaryStringRep PackageIdentifier")
      . simpleParse . fromStringRep
    toStringRep = toStringRep . display

instance GhcPkg.BinaryStringRep ModuleName where
    fromStringRep = ModuleName.fromString . fromStringRep
    toStringRep   = toStringRep . display

instance GhcPkg.BinaryStringRep String where
    fromStringRep = fromUTF8 . BS.unpack
    toStringRep   = BS.pack . toUTF8

instance GhcPkg.BinaryStringRep UnitId where
    fromStringRep = mkUnitId . fromStringRep
    toStringRep   = toStringRep . display

instance GhcPkg.DbUnitIdModuleRep UnitId ComponentId OpenUnitId ModuleName OpenModule where
    fromDbModule (GhcPkg.DbModule uid mod_name) = OpenModule uid mod_name
    fromDbModule (GhcPkg.DbModuleVar mod_name) = OpenModuleVar mod_name
    toDbModule (OpenModule uid mod_name) = GhcPkg.DbModule uid mod_name
    toDbModule (OpenModuleVar mod_name) = GhcPkg.DbModuleVar mod_name
    fromDbUnitId (GhcPkg.DbUnitId cid insts) = IndefFullUnitId cid (Map.fromList insts)
    fromDbUnitId (GhcPkg.DbInstalledUnitId uid)
      = DefiniteUnitId (unsafeMkDefUnitId uid)
    toDbUnitId (IndefFullUnitId cid insts) = GhcPkg.DbUnitId cid (Map.toList insts)
    toDbUnitId (DefiniteUnitId def_uid)
      = GhcPkg.DbInstalledUnitId (unDefUnitId def_uid)

