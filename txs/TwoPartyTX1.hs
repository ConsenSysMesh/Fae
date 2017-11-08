import Blockchain.Fae.Contracts

pubKey1 :: PublicKey
pubKey1 = "f5e73775fa3cf0556ee841404a9d6efe5b9c700f0cdbc096b76c773c17758522"

pubKey2 :: PublicKey
pubKey2 = "f4e0734d17249a9c2506f66324a4c2da9e0cac242aa515e796fe1d056fd0081b"

body :: Transaction Void ()
body _ = twoPartySwap pubKey1 pubKey2
