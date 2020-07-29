module Minithesis exposing
    ( Options
    , RandomRun
    , Test
    , TestResult(..)
    , defaultOptions
    , isFailsWith
    , run
    , runWith
    , test
    )

import Minithesis.Fuzz as Fuzz
import Minithesis.Fuzz.Internal as Fuzz exposing (Fuzzer)
import Minithesis.RandomRun as RandomRun
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase exposing (Status(..))
import Minithesis.TestingState as TestingState
import Minithesis.TestingState.Internal as TestingState
    exposing
        ( ShrinkCommand(..)
        , TestingState
        )
import Random


type alias Test a =
    TestingState.Test a


type alias RandomRun =
    -- the user facing one is List Int, but internally we're free to use a better data structure
    List Int


type TestResult a
    = Passes
    | FailsWith a
    | FailsWithShrinks
        { finalValue : a
        , finalRun : RandomRun
        , history :
            List
                { value : a
                , run : RandomRun
                , shrinkerUsed : String
                }
        }
    | Error Stop


type alias Options =
    { maxExamples : Int
    , showShrinkHistory : Bool
    }


defaultOptions : Options
defaultOptions =
    { maxExamples = 100
    , showShrinkHistory = False
    }


run : Int -> Test a -> ( String, TestResult a )
run seed test_ =
    runWith defaultOptions seed test_


runWith : Options -> Int -> Test a -> ( String, TestResult a )
runWith { maxExamples, showShrinkHistory } seed test_ =
    let
        state =
            TestingState.init
                (Random.initialSeed seed)
                maxExamples
                showShrinkHistory
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

        Ok finalState ->
            case finalState.bestCounterexample of
                Nothing ->
                    Passes

                Just counterexample ->
                    case Fuzz.run finalState.fuzzer (TestCase.forRun counterexample) of
                        Ok ( value, _ ) ->
                            if finalState.showShrinkHistory then
                                FailsWithShrinks
                                    { finalValue = value
                                    , finalRun = RandomRun.toList counterexample
                                    , history =
                                        finalState.shrinkHistory
                                            |> List.reverse
                                            |> List.map
                                                (\( value_, run_, maybeCmd ) ->
                                                    { value = value_
                                                    , run = RandomRun.toList run_
                                                    , shrinkerUsed =
                                                        case maybeCmd of
                                                            Nothing ->
                                                                "Initial"

                                                            Just cmd ->
                                                                shrinkCmdLabel cmd
                                                    }
                                                )
                                    }

                            else
                                FailsWith value

                        Err ( stop, _ ) ->
                            Error stop
    )


shrinkCmdLabel : ShrinkCommand -> String
shrinkCmdLabel cmd =
    case cmd of
        DeleteChunkAndMaybeDecrementPrevious { chunkSize, startIndex } ->
            "DeleteChunkAndMaybeDecrementPrevious { size = "
                ++ String.fromInt chunkSize
                ++ ", startIndex = "
                ++ String.fromInt startIndex
                ++ " }"

        ReplaceChunkWithZero { chunkSize, startIndex } ->
            "ReplaceChunkWithZero { size = "
                ++ String.fromInt chunkSize
                ++ ", startIndex = "
                ++ String.fromInt startIndex
                ++ " }"

        MinimizeChoiceWithBinarySearch { index } ->
            "MinimizeChoiceWithBinarySearch { index = "
                ++ String.fromInt index
                ++ " }"

        SortChunk { chunkSize, startIndex } ->
            "SortChunk { size = "
                ++ String.fromInt chunkSize
                ++ ", startIndex = "
                ++ String.fromInt startIndex
                ++ " }"

        RedistributeChoices { leftIndex, rightIndex } ->
            "RedistributeChoices { leftIndex = "
                ++ String.fromInt leftIndex
                ++ ", rightIndex = "
                ++ String.fromInt rightIndex
                ++ " }"


isFailsWith : TestResult a -> Bool
isFailsWith result =
    case result of
        FailsWith _ ->
            True

        _ ->
            False
