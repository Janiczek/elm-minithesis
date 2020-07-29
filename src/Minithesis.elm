module Minithesis exposing
    ( RandomRun
    , Test, test
    , Options, run, runWith, defaultOptions, TestResult(..)
    )

{-| `elm-minithesis` is a property-based testing library based on [Minithesis](https://github.com/drmaciver/minithesis), which is the minimal implementation of the core idea of [Hypothesis](https://github.com/HypothesisWorks/hypothesis).

What sets it apart from `elm-test` fuzzers is a different approach to shrinking.
Instead of shrinking the generated values, **Minithesis instead shrinks the PRNG
"rolled" values used to generate the values.** This makes the shrinking process
generic (you don't have to write a shrinker for your custom fuzzers) and
sidesteps issue that type-based shrinking has with monadic bind (`andThen`).

@docs RandomRun

@docs Test, test

@docs Options, run, runWith, defaultOptions, TestResult

-}

import Minithesis.Fuzz as Fuzz
import Minithesis.Fuzz.Internal as Fuzz exposing (Fuzzer)
import Minithesis.RandomRun as RandomRun
import Minithesis.Stop as Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase exposing (Status(..))
import Minithesis.TestingState as TestingState
import Minithesis.TestingState.Internal as TestingState
    exposing
        ( ShrinkCommand(..)
        , TestingState
        )
import Random


{-| Test holds all the info needed for Minithesis to start generating and
testing random values.
-}
type alias Test a =
    TestingState.Test a


{-| RandomRun can be thought of as a `List Int` - the values PRNG gave us in the
process of generating a value using a fuzzer.

Shrinkers work on such a RandomRun afterwards.

(You can see what RandomRun did your generated values create using the
`showShrinkHistory` field of the `runWith` function.)

-}
type alias RandomRun =
    -- the user facing one is List Int, but internally we're free to use a better data structure
    List Int


{-| A result of running the test with `run` or `runWith`.
-}
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
    | Unsatisfiable
    | InternalError Stop


{-| Options for running tests.

  - `maxExamples`: how many values to generate before concluding the test?
  - `showShrinkHistory`: return `FailsWithShrinks` instead of `FailsWith`

-}
type alias Options =
    { maxExamples : Int
    , showShrinkHistory : Bool
    }


{-|

    defaultOptions : Options
    defaultOptions =
        { maxExamples = 100
        , showShrinkHistory = False
        }

-}
defaultOptions : Options
defaultOptions =
    { maxExamples = 100
    , showShrinkHistory = False
    }


{-| Run a test with the given PRNG seed.

Note: if trying to use Minithesis inside `elm-explorations/test` suite, don't
wrap this function in `Test.fuzz` yourself. **Use `Test.Minithesis.mFuzz`
instead;** it sets sensible options (number of runs) on both levels.

-}
run : Int -> Test a -> ( String, TestResult a )
run seed test_ =
    runWith defaultOptions seed test_


{-| Run a test with the given PRNG seed and other options.

Note: if trying to use Minithesis inside `elm-explorations/test` suite, don't
wrap this function in `Test.fuzz` yourself. **Use `Test.Minithesis.mFuzz`
instead;** it sets sensible options (number of runs) on both levels.

-}
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


{-| This is how you can create a test for Minithesis.
Later use `run` or `runWith` to actually start generating and testing values.

    listsCanBeReversed : Test
    listsCanBeReversed =
        Minithesis.test "lists can always be reversed lol"
            (Fuzz.list (Fuzz.int 1 10))
            (\list -> List.reverse list == list)

-}
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
        Err ( Stop.Unsatisfiable, _ ) ->
            Unsatisfiable

        Err ( stop, _ ) ->
            InternalError stop

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
                            InternalError stop
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
