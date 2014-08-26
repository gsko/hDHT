{-# LANGUAGE OverloadedStrings #-}
module BencodeTest (tests) where
import DHT.Bencode

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import qualified Data.Map.Strict as M
import Text.Printf

tests = [
    testProperty "BInt encoding" bintEncode
  , testProperty "BStr encoding" bstrEncode
  , testCase "BList encoding" blistEncode
  , testCase "BDict encoding" bdictEncode
  , testProperty "BInt decoding" bintDecode
  , testProperty "BStr decode" bstrDecode
  , testCase "BStr decode empty" bstrEmpty
    ]

bintEncode :: Integer -> Property 
bintEncode n = property $ (BS.unpack . bshow . BInt) n ==
    printf "i%de" n

bstrEncode :: String -> Property
bstrEncode s = property $ (BS.unpack . bshow . BStr) s ==
    printf "%d:%s" (length s) s

blistEncode :: Assertion
blistEncode = "li5e3:heye" @?=
    (bshow . BList) [BInt 5, BStr "hey"]

bdictEncode :: Assertion
bdictEncode = "d1:ai1ee" @?=
    (bshow . BDict . M.fromList) [(BStr "a", BInt 1)]

bintDecode :: Integer -> Property
bintDecode n = case bdecode (printf "i%de" n) of
    Right ([BInt n']) -> property $ n == n'

bstrDecode :: String -> Property
bstrDecode s = case bdecode (printf "%d:%s" (length s) s) of
    Right ([BStr s']) -> property $ s == s'

bstrEmpty :: Assertion
bstrEmpty = case bdecode "0:" of
    Right ([BStr s]) -> s @?= ""
