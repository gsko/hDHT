module DHT.KBucket(
    offernode
  , removenode
  , nodeIn
  , empty
  , kbucket
  , nodes, minID, maxID, size
) where

import DHT.Node as N

data KBucket = KBucket {
    nodes :: [Node]
  , minID :: Word160
  , maxID :: Word160
  , size :: Integer
} deriving (Show)

kbucket :: [Node] -> Word160 -> Word160 -> Integer -> KBucket
kbucket nodes minID maxID size
    | minID > maxID = error "invalid min/max ID range"
    | size <= 0 = error "invalid size"
    | otherwise = KBucket nodes minID maxID size

empty :: Integer -> KBucket
empty = KBucket [] (N.fromInteger 0) (N.fromInteger $ 2^160-1)

offernode :: KBucket -> Node -> KBucket
offernode k@(KBucket nodes minID maxID size) node@(Node nodeID)
    | nodeID < minID || nodeID > maxID = k
    | elem node nodes = k
    | length nodes < fromIntegral size = KBucket (node:nodes) minID maxID size

removenode :: KBucket -> Node -> KBucket
removenode (KBucket nodes minID maxID size) node =
    let nodes' = filter (node/=) nodes in
        KBucket nodes' minID maxID size

nodeIn :: KBucket -> Node -> Bool
nodeIn k n = n `elem` (nodes k)
