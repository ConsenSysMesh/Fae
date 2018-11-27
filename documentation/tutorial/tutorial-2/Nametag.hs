module Nametag (Nametag, getNametag, checkNametag) where

import Blockchain.Fae
import Control.Monad (forever)

data NametagName = NametagName String deriving (Generic)
newtype Nametag = Nametag (EscrowID NametagName) deriving (Generic)

instance ContractName NametagName where
  type ArgType NametagName = ()
  type ValType NametagName = String
  theContract (NametagName name) = \_ -> forever $ release $ "Property of: " ++ name
  
getNametag :: (MonadTX m) => Reward -> String -> m Nametag
getNametag rwd name = do
  claimReward rwd
  Nametag <$> newEscrow (NametagName name)

checkNametag :: (MonadTX m) => Nametag -> m String
checkNametag (Nametag eID) = useEscrow [] eID ()
