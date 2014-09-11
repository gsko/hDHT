import Test.Framework
import Test.Framework.Options
import Data.Monoid

import qualified BencodeTest as BT
import qualified NodeTest as NT
import qualified KBucketTest as KT

main :: IO ()
main = do
    let opts = (mempty :: TestOptions) {
        topt_maximum_generated_tests = Just 1000
    }
    let runnerOpts = (mempty :: RunnerOptions) {
        ropt_test_options = Just opts
      , ropt_hide_successes = Just False
      , ropt_color_mode = Just ColorAuto
      , ropt_list_only = Just False
    }
    let tests = [BT.tests, NT.tests, KT.tests]
    defaultMainWithOpts tests runnerOpts
