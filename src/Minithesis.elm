module Minithesis exposing
    ( Options
    , Test
    , TestResult(..)
    , defaultOptions
    , isFailsWith
    , run
    , runWith
    , test
    )

import Minithesis.Fuzz as Fuzz exposing (Fuzzer)
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase exposing (Status(..))
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


run : Int -> Test a -> ( String, TestResult a )
run seed test_ =
    runWith defaultOptions seed test_


runWith : Options -> Int -> Test a -> ( String, TestResult a )
runWith { maxExamples } seed test_ =
    let
        state =
            TestingState.init
                (Random.initialSeed seed)
                maxExamples
                test_
    in
    runState state


test : String -> Fuzzer a -> (a -> Bool) -> Test a
test label fuzzer userTestFn =
    TestingState.Test
        { label = label
        , fuzzer = fuzzer
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


runState : TestingState a -> ( String, TestResult a )
runState state =
    ( state.label
    , case
        Ok state
            |> TestingState.generate
            |> TestingState.stopIfUnsatisfiable
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
    )


isFailsWith : TestResult a -> Bool
isFailsWith result =
    case result of
        FailsWith _ ->
            True

        _ ->
            False
