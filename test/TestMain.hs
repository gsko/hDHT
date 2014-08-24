{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import qualified Data.Map.Strict as M
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Text.Printf
import qualified Data.ByteString.Char8 as BS

import DHT.Bencode

main :: IO ()
main = defaultMainWithOpts [
         testProperty "BInt encoding" bintEncode
       , testProperty "BStr encoding" bstrEncode
       , testCase "BList encoding" blistEncode
       , testCase "BDict encoding" bdictEncode
       , testCase "BInt decoding" bintDecode
       ] mempty

bintEncode :: Integer -> Property 
bintEncode n = property $ (BS.unpack . bshow . BInt) n ==
    "i" ++ show n ++ "e"

bstrEncode :: String -> Property
bstrEncode s = property $  (BS.unpack . bshow . BStr) s ==
    (show . length) s ++ ":" ++ s

blistEncode :: Assertion
blistEncode = bshow (BList [BInt 5, BStr "hey"]) @?= "li5e3:heye"

bdictEncode :: Assertion
bdictEncode = bshow (BDict (M.fromList [(BStr "a", BInt 1)])) @?=
    "d1:ai1ee"

bintDecode :: Assertion
bintDecode = case bdecode "i5e" of
    Right ([BInt i]) -> i @?= 5
