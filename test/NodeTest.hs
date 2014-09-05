module NodeTest (tests) where

import DHT.Node

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

tests = TestGroup "NodeTest" [
    testCase "test: node construction"
        (Node 5 @?= Node 5)
    ]
