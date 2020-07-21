module MinithesisTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Minithesis exposing (TestResult(..))
import Minithesis.Fuzz as MFuzz exposing (Fuzzer)
import Minithesis.Stop exposing (Stop(..))
import Test


rejectingAllFuzzer : Fuzzer Int
rejectingAllFuzzer =
    MFuzz.nonnegativeInt 10
        |> MFuzz.filter (\_ -> False)


rejecting0Fuzzer : Fuzzer Int
rejecting0Fuzzer =
    MFuzz.nonnegativeInt 10
        |> MFuzz.filter (\n -> n /= 0)


suite : Test.Test
suite =
    Test.describe "Fuzz.filter"
        [ Test.fuzz Fuzz.int "Stops with Unsatisfiable if rejecting too many values" <|
            \seed ->
                Minithesis.test rejectingAllFuzzer (\_ -> True)
                    |> Minithesis.run seed
                    |> Expect.equal (Error Unsatisfiable)
        , Test.fuzz Fuzz.int "Generated values satisfy preconditions" <|
            \seed ->
                Minithesis.test rejecting0Fuzzer (\n -> n /= 0)
                    |> Minithesis.run seed
                    |> Expect.equal Passes
        ]
