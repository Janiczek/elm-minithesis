module Minithesis.Fuzz.Internal exposing (Fuzzer(..), run)

import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase exposing (Status(..), TestCase)


type Fuzzer a
    = Fuzzer (TestCase -> Result ( Stop, TestCase ) ( a, TestCase ))


run : Fuzzer a -> TestCase -> Result ( Stop, TestCase ) ( a, TestCase )
run (Fuzzer fn) testCase =
    fn testCase
