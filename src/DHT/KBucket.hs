module DHT.KBucket where

import DHT.Node as N

data KBucket = KBucket {
    nodes :: [Node]
  , minID :: Word160
  , maxID :: Word160
  , size :: Integer
} deriving (Show)

empty :: Integer -> KBucket
empty = KBucket [] (N.fromInteger 0) (N.fromInteger $ 2^160)

offernode :: KBucket -> Node -> KBucket
offernode k@(KBucket nodes minID maxID size) node@(Node nodeID)
    | (nodeID < minID || nodeID > maxID) = k
    | (length nodes) < fromIntegral size = KBucket (node:nodes) minID maxID size