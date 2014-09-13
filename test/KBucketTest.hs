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
x = KBucket [] (N.fromInteger 0) (N.fromInteger $ 2^160) 256

tests = TestGroup "KBucketTest" []
