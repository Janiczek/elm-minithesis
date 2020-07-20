module Minithesis.TestingState exposing
    ( Test(..)
    , TestingState
    , generate
    , init
    , shrink
    )

import Minithesis.Fuzz exposing (Fuzzer)
import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , Stop(..)
        , TestCase
        )
import OurExtras.List
import Random


type Test a
    = Test
        { userTestFn : TestCase -> Result ( Stop, TestCase ) ( Bool, TestCase )
        , fuzzer : Fuzzer a
        }


type alias TestingState a =
    -- config
    { seed : Random.Seed
    , maxExamples : Int
    , userTestFn : TestCase -> Result ( Stop, TestCase ) TestCase
    , fuzzer : Fuzzer a

    -- state
    , validTestCases : Int
    , calls : Int
    , bestCounterexample : Maybe RandomRun
    }


{-| We cap the maximum amount of entropy a test case can use. This prevents
cases where the generated test case size explodes by effectively rejection.
-}
bufferSize : Int
bufferSize =
    8 * 1024


init :
    Random.Seed
    -> Int
    -> Test a
    -> TestingState a
init seed maxExamples (Test { userTestFn, fuzzer }) =
    { seed = seed
    , maxExamples = maxExamples
    , userTestFn = markFailuresInteresting userTestFn
    , fuzzer = fuzzer
    , validTestCases = 0
    , calls = 0
    , bestCounterexample = Nothing
    }


markFailuresInteresting :
    (TestCase -> Result ( Stop, TestCase ) ( Bool, TestCase ))
    -> TestCase
    -> Result ( Stop, TestCase ) TestCase
markFailuresInteresting userTestFn testCase =
    case userTestFn testCase of
        Err err ->
            Err err

        Ok ( True, testCase_ ) ->
            Ok testCase_

        Ok ( False, testCase_ ) ->
            testCase_
                |> TestCase.markStatus Interesting


generate :
    Result ( Stop, TestCase ) (TestingState a)
    -> Result ( Stop, TestCase ) (TestingState a)
generate result =
    case result of
        Err _ ->
            result

        Ok state ->
            if
                (state.bestCounterexample == Nothing)
                    && (state.validTestCases < state.maxExamples)
                    && (state.calls < state.maxExamples * 10)
            then
                generate
                    (runTest
                        (TestCase.init
                            { prefix = RandomRun.empty
                            , seed = state.seed
                            , maxSize = bufferSize
                            }
                        )
                        state
                        |> Result.map Tuple.first
                    )

            else
                Ok state


runTest :
    TestCase
    -> TestingState a
    -> Result ( Stop, TestCase ) ( TestingState a, TestCase )
runTest testCase state =
    let
        go : TestCase -> TestingState a
        go testCase_ =
            { state
                | calls = state.calls + 1
                , validTestCases =
                    if
                        List.member testCase_.status
                            [ Valid
                            , Interesting
                            ]
                    then
                        state.validTestCases + 1

                    else
                        state.validTestCases
                , bestCounterexample =
                    if testCase_.status == Interesting then
                        case state.bestCounterexample of
                            Nothing ->
                                Just testCase_.randomRun

                            Just bestCounterexample ->
                                if RandomRun.compare testCase_.randomRun bestCounterexample == LT then
                                    Just testCase_.randomRun

                                else
                                    state.bestCounterexample

                    else
                        state.bestCounterexample
                , seed =
                    testCase_.seed
                        |> Maybe.withDefault state.seed
            }
    in
    case
        testCase
            |> state.userTestFn
            |> Result.andThen markValidIfUndecided
    of
        Ok testCase_ ->
            Ok ( go testCase_, testCase_ )

        Err ( StopTest, testCase_ ) ->
            Ok ( go testCase_, testCase_ )

        Err otherErr ->
            Err otherErr


markValidIfUndecided : TestCase -> Result ( Stop, TestCase ) TestCase
markValidIfUndecided testCase =
    if testCase.status == Undecided then
        testCase
            |> TestCase.markStatus Valid

    else
        Ok testCase


shrink :
    Result ( Stop, TestCase ) (TestingState a)
    -> Result ( Stop, TestCase ) (TestingState a)
shrink result =
    case result of
        Ok state ->
            case state.bestCounterexample of
                Nothing ->
                    result

                Just counterexample ->
                    Ok <| iterateShrinkWhileProgress counterexample state

        Err _ ->
            result


iterateShrinkWhileProgress : RandomRun -> TestingState a -> TestingState a
iterateShrinkWhileProgress counterexample state =
    case shrinkOnce counterexample state of
        Err _ ->
            state

        Ok ( nextCounterexample, nextState ) ->
            if nextCounterexample == counterexample then
                nextState

            else
                iterateShrinkWhileProgress nextCounterexample nextState


type ShrinkCommand
    = DeleteChunk { chunkSize : Int, startIndex : Int }
    | ReplaceChunkWithZero { chunkSize : Int, startIndex : Int }
    | MinimizeChoiceWithBinarySearch { index : Int }


shrinkOnce :
    RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( RandomRun, TestingState a )
shrinkOnce counterexample state =
    let
        shrinkCommands =
            allShrinks counterexample
    in
    runShrinkCommands shrinkCommands counterexample state


runShrinkCommand :
    ShrinkCommand
    -> RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( TestingState a, TestCase )
runShrinkCommand cmd randomRun state =
    case cmd of
        DeleteChunk meta ->
            runTest
                (TestCase.forRun (RandomRun.deleteChunk meta randomRun))
                state

        ReplaceChunkWithZero meta ->
            runTest
                (TestCase.forRun (RandomRun.replaceChunkWithZero meta randomRun))
                state

        MinimizeChoiceWithBinarySearch { index } ->
            let
                testCase =
                    TestCase.forRun randomRun
            in
            RandomRun.get index randomRun
                |> Maybe.map
                    (\value ->
                        loopShrink
                            binarySearch
                            { low = 0
                            , high = value
                            , index = index
                            }
                            randomRun
                            state
                    )
                |> Maybe.withDefault (Ok ( state, testCase ))


type Loop a
    = TryThisNext RandomRun (Bool -> a)
    | Stop


loopShrink :
    (loopState -> RandomRun -> Loop loopState)
    -> loopState
    -> RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( TestingState a, TestCase )
loopShrink shrinkFn loopState randomRun state =
    case shrinkFn loopState randomRun of
        TryThisNext nextRandomRun toLoopState ->
            case runTest (TestCase.forRun nextRandomRun) state of
                Err err ->
                    Err err

                Ok ( nextState, nextTestCase ) ->
                    let
                        isInteresting : Bool
                        isInteresting =
                            nextTestCase.status == Interesting
                    in
                    loopShrink shrinkFn (toLoopState isInteresting) nextRandomRun nextState

        Stop ->
            Ok ( state, TestCase.forRun randomRun )


type alias BinarySearchState =
    { index : Int
    , low : Int
    , high : Int
    }


binarySearch : BinarySearchState -> RandomRun -> Loop BinarySearchState
binarySearch ({ index, low, high } as state) randomRun =
    if low + 1 < high then
        let
            mid =
                low + (high - low) // 2

            newRandomRun =
                RandomRun.set index mid randomRun
        in
        TryThisNext
            newRandomRun
            (\wasInteresting ->
                if wasInteresting then
                    { state | high = mid }

                else
                    { state | low = mid }
            )

    else
        Stop


runShrinkCommands :
    List ShrinkCommand
    -> RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( RandomRun, TestingState a )
runShrinkCommands cmds randomRun state =
    List.foldl
        (\cmd result ->
            case result of
                Err _ ->
                    result

                Ok ( accRandomRun, accState ) ->
                    runShrinkCommand cmd accRandomRun accState
                        |> Result.andThen
                            (\( newState, testCase ) ->
                                case newState.bestCounterexample of
                                    Nothing ->
                                        Err ( LostCounterexample, testCase )

                                    Just newChoice ->
                                        Ok ( newChoice, newState )
                            )
        )
        (Ok ( randomRun, state ))
        cmds


allShrinks : RandomRun -> List ShrinkCommand
allShrinks counterexample =
    let
        metadata =
            { itemsCount = RandomRun.length counterexample }
    in
    OurExtras.List.fastConcat
        [ deletionShrinks metadata
        , zeroShrinks metadata
        , binarySearchShrinks metadata
        ]


deletionShrinks : { itemsCount : Int } -> List ShrinkCommand
deletionShrinks =
    blockShrinks DeleteChunk


zeroShrinks : { itemsCount : Int } -> List ShrinkCommand
zeroShrinks =
    blockShrinks ReplaceChunkWithZero


binarySearchShrinks : { itemsCount : Int } -> List ShrinkCommand
binarySearchShrinks { itemsCount } =
    List.range 0 (itemsCount - 1)
        |> List.reverse
        |> List.map (\index -> MinimizeChoiceWithBinarySearch { index = index })


{-| Not entirely an exact port of the Python Minithesis behaviour,
eg. it doesn't retry deleting at the same index after deleting a chunk.

    blockShrinks
        DeleteChunk
        { itemsCount = 10 }
        -->
        [ -- Chunks of size 8
          DeleteChunk { chunkSize = 8, startIndex = 2 }
        , DeleteChunk { chunkSize = 8, startIndex = 1 }
        , DeleteChunk { chunkSize = 8, startIndex = 0 }

        -- Chunks of size 4
        , DeleteChunk { chunkSize = 4, startIndex = 6 }
        , DeleteChunk { chunkSize = 4, startIndex = 5 }
        , -- ...
          DeleteChunk { chunkSize = 4, startIndex = 1 }
        , DeleteChunk { chunkSize = 4, startIndex = 0 }

        -- Chunks of size 2
        , DeleteChunk { chunkSize = 2, startIndex = 8 }
        , DeleteChunk { chunkSize = 2, startIndex = 7 }
        , -- ...
          DeleteChunk { chunkSize = 2, startIndex = 1 }
        , DeleteChunk { chunkSize = 2, startIndex = 0 }

        -- Chunks of size 1
        , DeleteChunk { chunkSize = 1, startIndex = 9 }
        , DeleteChunk { chunkSize = 1, startIndex = 8 }
        , -- ...
          DeleteChunk { chunkSize = 1, startIndex = 1 }
        , DeleteChunk { chunkSize = 1, startIndex = 0 }
        ]

-}
blockShrinks : ({ chunkSize : Int, startIndex : Int } -> a) -> { itemsCount : Int } -> List a
blockShrinks toShrink { itemsCount } =
    let
        go : Int -> Int -> List a -> List a
        go chunkSize startIndex acc =
            if startIndex > itemsCount - chunkSize then
                if chunkSize == 8 then
                    acc

                else
                    go (chunkSize * 2) 0 acc

            else
                let
                    newCommand =
                        toShrink
                            { chunkSize = chunkSize
                            , startIndex = startIndex
                            }
                in
                go chunkSize (startIndex + 1) (newCommand :: acc)
    in
    go 1 0 []
