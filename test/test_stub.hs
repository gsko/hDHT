import Test.HUnit

test1 = TestCase $ assertEqual "Tuple Equality" (1,2) (2,3)

tests = TestList [TestLabel "test1" test1]
main = runTestTT tests
