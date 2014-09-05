module DHT.Node where

data Node = Node {
    id :: Integer
} deriving (Show, Eq)

nodeDist :: Node -> Node -> Integer
nodeDist (Node n) (Node n') = n ^ n'
