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
         testCase "bshow BInt" bshowTest
       , testProperty "bshow BInt Length" bShowIntProp 
       ] mempty

bshowTest :: Assertion
bshowTest = bshow (BInt 5) @?= "i5e"

bShowIntProp :: Integer -> Property
bShowIntProp i = property $ (BS.length . bshow . BInt) i == 2 + (length . show) i
