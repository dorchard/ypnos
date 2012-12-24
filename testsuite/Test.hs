import Test.Framework 
import Test.Framework.Options

import Data.Monoid

import Testing.Ypnos.CUDA.Expr.Fun

main = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions

  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts { 
                      topt_maximum_generated_tests = Just 100
                    , topt_maximum_unsuitable_generated_tests = Just 200
                    , topt_timeout = Just $ Just 10000 }

  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }

  defaultMainWithOpts tests my_runner_opts

tests = [ fun_tests ]
