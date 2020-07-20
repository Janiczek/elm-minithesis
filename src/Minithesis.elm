module Minithesis exposing
    ( Options
    , Test
    , TestResult(..)
    , defaultOptions
    , run
    , runWith
    , test
    )

import Minithesis.Fuzz as Fuzz exposing (Fuzzer)
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , Stop
        )
import Minithesis.TestingState as TestingState exposing (TestingState)
import Random


type alias Test a =
    TestingState.Test a


type TestResult a
    = Passes
    | FailsWith a
    | Error Stop


type alias Options =
    { maxExamples : Int }


defaultOptions : Options
defaultOptions =
    { maxExamples = 100 }


run : Int -> Test a -> TestResult a
run seed test_ =
    runWith defaultOptions seed test_


runWith : Options -> Int -> Test a -> TestResult a
runWith { maxExamples } seed test_ =
    let
        state =
            TestingState.init
                (Random.initialSeed seed)
                maxExamples
                test_
    in
    runState state


test : Fuzzer a -> (a -> Bool) -> Test a
test fuzzer userTestFn =
    TestingState.Test
        { fuzzer = fuzzer
        , userTestFn =
            \testCase ->
                case Fuzz.run fuzzer testCase of
                    Err err ->
                        Err err

                    Ok ( value, testCase_ ) ->
                        Ok
                            ( userTestFn value
                            , testCase_
                            )
        }


runState : TestingState a -> TestResult a
runState state =
    case
        Ok state
            |> TestingState.generate
            |> TestingState.shrink
    of
        Err ( stop, _ ) ->
            Error stop

        Ok { bestCounterexample } ->
            case bestCounterexample of
                Nothing ->
                    Passes

                Just counterexample ->
                    case Fuzz.run state.fuzzer (TestCase.forRun counterexample) of
                        Ok ( value, _ ) ->
                            FailsWith value

                        Err ( stop, _ ) ->
                            Error stop
