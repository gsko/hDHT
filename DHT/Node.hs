module DHT.Node where

import Data.Word
import Data.Bits

-- Word160 : 160 bits = 20 bytes = 5 * 4 byte words = 5 Word32's
data Word160 = Word160 !Word32 !Word32 !Word32 !Word32 !Word32
    deriving (Show, Eq)

data Node = Node {
    id :: Integer
} deriving (Show, Eq)

dist :: Word160 -> Word160 -> Word160
dist (Word160 a b c d e) (Word160 a' b' c' d' e') = Word160 (a+a') (b+b') (c+c') (d+d') (e+e')

toInteger :: Word160 -> Integer
toInteger (Word160 a b c d e) =
    (fromIntegral $ a `shiftL` (4*4*8))
  + (fromIntegral $ b `shiftL` (3*4*8))
  + (fromIntegral $ c `shiftL` (2*4*8))
  + (fromIntegral $ d `shiftL` (1*4*8))
  + (fromIntegral e)
