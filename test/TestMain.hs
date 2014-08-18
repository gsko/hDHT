{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import qualified Data.ByteString as BS

import DHT.Bencode

main :: IO ()
main = defaultMainWithOpts [
         testCase "bshow BInt" bshowTest
       , testProperty "bshow BInt Length" True
       ] mempty

bshowTest :: Assertion
bshowTest = bshow (BInt 5) @?= "i5e"

bshowProp :: BVal -> Bool
bshowProp b@(BInt i) = (BS.length . bshow) b == 0

-- propListRevRevId :: [Int] -> Property
-- propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs
