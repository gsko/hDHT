module BencodeTest (tests) where
import DHT.Bencode

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import qualified Data.Map.Strict as M
import Text.Printf

tests = TestGroup "BencodeTest" [
    testProperty "BInt encoding" prop_bencodeBInt
  -- , testProperty "BStr encoding" prop_bencodeBStr
  , testCase "BList encoding" prop_bencodeBList
  , testCase "BDict encoding" prop_bencodeBDict
  , testProperty "BInt decoding" prop_bdecodeBInt
  , testProperty "BStr decode" prop_bdecodeBStr
  , testCase "BStr decode empty" test_bdecodeZeroLenBStr
    ]

prop_bencodeBInt :: Integer -> Property 
prop_bencodeBInt n = property $ (B.unpack . bencode . BInt) n ==
    printf "i%de" n

-- TODO update this test
-- prop_bencodeBStr :: B.ByteString -> Property
-- prop_bencodeBStr s = property $ (bencode . BStr) s ==
--     ((C.pack . show . B.length) $ s) ~~ ":" ~~ s

prop_bencodeBList :: Assertion
prop_bencodeBList = "li5e3:heye" @?=
    (bencode . BList) [BInt 5, BStr "hey"]

prop_bencodeBDict :: Assertion
prop_bencodeBDict = "d1:ai1ee" @?=
    (bencode . BDict . M.fromList) [(BStr "a", BInt 1)]

prop_bdecodeBInt :: Integer -> Property
prop_bdecodeBInt n = case bdecode . C.pack $ (printf "i%de" n) of
    Just ([BInt n']) -> property $ n == n'

prop_bdecodeBStr :: String -> Property
prop_bdecodeBStr s = case bdecode . C.pack $ printf "%d:%s" (length s) s of
    Just ([BStr s']) -> property $ s == C.unpack s'

test_bdecodeZeroLenBStr :: Assertion
test_bdecodeZeroLenBStr = case bdecode "0:" of
    Just ([BStr s]) -> s @?= ""
