module DHT.Node where

import Data.Word
import Data.Bits

-- Word160 : 160 bits = 20 bytes = 5 * 4 byte words = 5 Word32's
data Word160 = Word160 !Word32 !Word32 !Word32 !Word32 !Word32
    deriving (Show, Eq)

instance Bounded Word160 where
    minBound = Word160 0 0 0 0 0
    maxBound = Word160 1 1 1 1 1
    --    where m :: Data.Word.Word32
    --          m = maxBound Data.Word.Word32

data Node = Node {
    id :: Word160
} deriving (Show, Eq)

dist :: Word160 -> Word160 -> Word160
dist (Word160 a b c d e) (Word160 a' b' c' d' e') =
    Word160 (a `xor` a') (b `xor` b') (c `xor` c') (d `xor` d') (e `xor` e')

toInteger :: Word160 -> Integer
toInteger (Word160 a b c d e) =
    (Prelude.toInteger a) `shiftL` (4*32)
  + (Prelude.toInteger b) `shiftL` (3*32)
  + (Prelude.toInteger c) `shiftL` (2*32)
  + (Prelude.toInteger d) `shiftL` (1*32)
  + (Prelude.toInteger e) `shiftL` (0*32)
