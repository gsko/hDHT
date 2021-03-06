module NodeTest (tests) where

import Data.ByteString as B
import DHT.Node as N
import Data.Word

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

instance Arbitrary N.Node where
    arbitrary = arbitraryNode

arbitraryNode :: Gen N.Node
arbitraryNode = do
    nodeID <- arbitraryWord160
    return $ (Node nodeID)

instance Arbitrary Word160 where
    arbitrary = arbitraryWord160

arbitraryWord160 :: Gen Word160
arbitraryWord160 = do
    let minW32 = fromIntegral $ (minBound :: Word32)
        maxW32 = fromIntegral $ (maxBound :: Word32)
    a <- choose (minW32, maxW32)
    b <- choose (minW32, maxW32)
    c <- choose (minW32, maxW32)
    d <- choose (minW32, maxW32)
    e <- choose (minW32, maxW32)
    return $ (Word160 a b c d e)

tests = TestGroup "NodeTest" [
    testCase "test: Word160 construction" (Word160 0 0 0 0 5 @?= Word160 0 0 0 0 5)
  , testCase "test: dist between same id is 0" (
        let w = Word160 123 456 789 1011 1213 in
            dist w w @?= Word160 0 0 0 0 0)
  , testCase "test: fromInteger 5+5*2^32" (
        N.fromInteger (5+5*2^32) @?= Word160 0 0 0 5 5)
  , testCase "test: maxBound of Word160" (
        let f :: Integer -> Integer -> Integer
            f acc n = acc + ((2^32 - 1) * 2^n) in
        (maxBound :: Word160) @?= (N.fromInteger $ Prelude.foldl f 0 [128,96,64,32,0]))
  , testProperty "prop: Nodes with same ID have distance zero"
        (\(Node n) -> property $ dist n n == Word160 0 0 0 0 0)
  , testCase "test: Word160->Integer 0 equality"
        (N.toInteger (Word160 0 0 0 0 0) @?= 0)
  , testCase "test: Word160->Integer 1 equality"
        (N.toInteger (Word160 0 0 0 0 1) @?= 1)
  , testCase "test: Word160->Integer 2^32 equality"
        (N.toInteger (Word160 0 0 0 1 0) @?= 2^32)
  , testCase "test: Word160 can't be negative"
        (N.toInteger (Word160 0 0 0 0 (-1)) < 0 @?= False)
  , testCase "test: big Word160->Integer"
        (N.toInteger (Word160 1 1 1 1 1) @?= 2^0 + 2^32 + 2^64 + 2^96 + 2^128)
--  , testCase "test: serialize word160 to bytestring"
--        (N.toBS (Word160 1 1 1 1 1) @?= B.concat 

  -- TODO:: figure out how to assert an 'error' with HUnit
  --, testCase "test: fromInteger overflow case"
    ]
