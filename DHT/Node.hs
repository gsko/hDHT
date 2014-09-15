module DHT.Node where

import Data.Word
import Data.Bits

-- Word160 : 160 bits = 20 bytes = 5 * 4 byte words = 5 Word32's
data Word160 = Word160 !Word32 !Word32 !Word32 !Word32 !Word32
    deriving (Show, Eq, Ord)

instance Bounded Word160 where
    minBound = Word160 0 0 0 0 0
    maxBound = Word160 m m m m m
        where m :: Word32
              m = maxBound :: Word32

data Node = Node {
    id :: Word160
} deriving (Show, Eq)

dist :: Word160 -> Word160 -> Word160
dist (Word160 a b c d e) (Word160 a' b' c' d' e') =
    Word160 (a `xor` a') (b `xor` b') (c `xor` c') (d `xor` d') (e `xor` e')

fromInteger :: Integer -> Word160
fromInteger i
    | i < 0 || i > 2^160 = 
        error ("integer is <0 or >2^160 : " ++ show i)
    | otherwise = (
        let max_w32 = maxBound :: Word32
            mask n = ((fromIntegral n) .&. max_w32) :: Word32
            w1 = mask $ i `shiftR` 128
            w2 = mask $ i `shiftR` 96
            w3 = mask $ i `shiftR` 64
            w4 = mask $ i `shiftR` 32
            w5 = mask $ i `shiftR` 0
        in Word160 w1 w2 w3 w4 w5)

toInteger :: Word160 -> Integer
toInteger (Word160 a b c d e) =
    (Prelude.toInteger a) `shiftL` (4*32)
  + (Prelude.toInteger b) `shiftL` (3*32)
  + (Prelude.toInteger c) `shiftL` (2*32)
  + (Prelude.toInteger d) `shiftL` (1*32)
  + (Prelude.toInteger e) `shiftL` (0*32)
