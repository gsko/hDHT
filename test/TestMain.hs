import Test.Framework
import Test.Framework.Options
import Data.Monoid

import qualified BencodeTest as BT

main :: IO ()
main = do
    let opts = (mempty :: TestOptions) {
        topt_maximum_generated_tests = Just 1000
    }
    let runnerOpts = (mempty :: RunnerOptions) {
        ropt_test_options = Just opts
      , ropt_hide_successes = Just True
      , ropt_color_mode = Just ColorAlways
      , ropt_list_only = Just False
    }
    defaultMainWithOpts BT.tests runnerOpts
