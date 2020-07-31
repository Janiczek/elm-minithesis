module Minithesis.TestingState.Internal exposing
    ( ShrinkCommand(..)
    , TestingState
    , runTest
    )

import Dict exposing (Dict)
import Minithesis.Fuzz.Internal exposing (Fuzzer)
import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase exposing (Status(..), TestCase)
import Random


type alias TestingState a =
    -- config
    { seed : Random.Seed
    , maxExamples : Int
    , showShrinkHistory : Bool
    , label : String
    , userTestFn : TestCase -> Result ( Stop, TestCase ) TestCase
    , fuzzer : Fuzzer a

    -- state
    , validTestCases : Int
    , calls : Int
    , rejections : Dict String Int
    , bestCounterexample : Maybe RandomRun
    , previousBestCounterexample : Maybe RandomRun
    , shrinkHistory : List ( a, RandomRun, Maybe ShrinkCommand )
    }


type ShrinkCommand
    = DeleteChunkAndMaybeDecrementPrevious { chunkSize : Int, startIndex : Int }
    | ReplaceChunkWithZero { chunkSize : Int, startIndex : Int }
    | MinimizeChoiceWithBinarySearch { index : Int }
    | SortChunk { chunkSize : Int, startIndex : Int }
    | RedistributeChoices { leftIndex : Int, rightIndex : Int }


runTest :
    TestCase
    -> TestingState a
    -> Result ( Stop, TestCase ) ( TestingState a, TestCase )
runTest testCase state =
    let
        toNewState : TestCase -> TestingState a
        toNewState testCase0 =
            let
                testCase1 =
                    markValidIfUndecided testCase0
            in
            { state
                | calls = state.calls + 1
                , validTestCases =
                    if List.member testCase1.status [ Valid, Interesting ] then
                        state.validTestCases + 1

                    else
                        state.validTestCases
                , bestCounterexample =
                    if testCase1.status == Interesting then
                        case state.bestCounterexample of
                            Nothing ->
                                Just testCase1.randomRun

                            Just bestCounterexample ->
                                if RandomRun.compare testCase1.randomRun bestCounterexample == LT then
                                    Just testCase1.randomRun

                                else
                                    state.bestCounterexample

                    else
                        state.bestCounterexample
                , seed =
                    testCase1.seed
                        |> Maybe.withDefault state.seed
                , shrinkHistory =
                    if state.showShrinkHistory then
                        state.shrinkHistory

                    else
                        state.shrinkHistory
                , rejections =
                    case testCase1.status of
                        Invalid { rejection } ->
                            state.rejections
                                |> Dict.update rejection
                                    (\maybeN ->
                                        case maybeN of
                                            Just n ->
                                                Just (n + 1)

                                            Nothing ->
                                                Just 1
                                    )

                        _ ->
                            state.rejections
            }
    in
    if state.bestCounterexample == Just testCase.prefix then
        Ok ( state, testCase )

    else
        case state.userTestFn testCase of
            Ok testCase_ ->
                Ok ( toNewState testCase_, testCase_ )

            Err ( StopTest, testCase_ ) ->
                Ok ( toNewState testCase_, testCase_ )

            Err otherErr ->
                Err otherErr


markValidIfUndecided : TestCase -> TestCase
markValidIfUndecided testCase =
    if testCase.status == Undecided then
        { testCase | status = Valid }

    else
        testCase
