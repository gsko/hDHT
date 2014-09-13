module DHT.KBucket where

import DHT.Node

data KBucket = KBucket {
    nodes :: [Node]
  , minID :: Word160
  , maxID :: Word160
  , size :: Integer
} deriving (Show)
