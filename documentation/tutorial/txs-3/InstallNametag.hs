import Nametag

body :: FaeTX ()
body = newContract OfferNametag

data OfferNametag = OfferNametag deriving (Generic)

instance ContractName OfferNametag where
  type ArgType OfferNametag = String
  type ValType OfferNametag = EscrowID GetNametag
  theContract OfferNametag = \name -> do
    eID <- newEscrow $ GetNametag name
    release eID >>= theContract OfferNametag
    
data GetNametag = GetNametag String deriving (Generic)

instance ContractName GetNametag where
  type ArgType GetNametag = Reward
  type ValType GetNametag = Nametag
  theContract (GetNametag name) = \rwd -> do
    nt <- getNametag rwd name
    release nt >>= theContract (GetNametag name)
