{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Text.Printf
import qualified Data.ByteString.Char8 as BS

import DHT.Bencode

instance Show BVal where
    show = (BS.unpack . bshow)

main :: IO ()
main = defaultMainWithOpts [
         testProperty "BInt encoding property" bintEncode
       , testProperty "BStr encoding property" bstrEncode
       , testProperty "BList encoding property" blistEncode
       ] mempty

bintEncode :: Integer -> Property 
bintEncode n = property $ (BS.unpack . bshow . BInt) n == "i" ++ show n ++ "e"

bstrEncode :: String -> Property
bstrEncode s = property $  (BS.unpack . bshow . BStr) s == (show . length) s ++ ":" ++ s

blistEncode :: [RawBVals] -> Property
blistEncode l = property $ True