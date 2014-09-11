module KBucketTest (tests) where
import DHT.KBucket
import DHT.Node as N

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

x :: KBucket
x = KBucket $ map (Node . N.fromInteger) [1,2,3,4,5]

tests = TestGroup "KBucketTest" []
