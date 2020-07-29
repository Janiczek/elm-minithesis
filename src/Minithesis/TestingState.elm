module Minithesis.TestingState exposing
    ( Test(..)
    , TestingState
    , generate
    , init
    , shrink
    , stopIfUnsatisfiable
    )

import Minithesis.Fuzz exposing (Fuzzer)
import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , TestCase
        )
import OurExtras.List
import Random


type Test a
    = Test
        { label : String
        , userTestFn : TestCase -> Result ( Stop, TestCase ) ( Bool, TestCase )
        , fuzzer : Fuzzer a
        }


type alias TestingState a =
    -- config
    { seed : Random.Seed
    , maxExamples : Int
    , label : String
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
init seed maxExamples (Test { label, userTestFn, fuzzer }) =
    { seed = seed
    , maxExamples = maxExamples
    , label = label
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


stopIfUnsatisfiable :
    Result ( Stop, TestCase ) (TestingState a)
    -> Result ( Stop, TestCase ) (TestingState a)
stopIfUnsatisfiable result =
    case result of
        Err err ->
            result

        Ok state ->
            if state.validTestCases == 0 then
                Err
                    ( Unsatisfiable
                    , TestCase.forRun (Maybe.withDefault RandomRun.empty state.bestCounterexample)
                    )

            else
                result


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
                    if RandomRun.isEmpty counterexample then
                        result

                    else
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
    | SortChunk { chunkSize : Int, startIndex : Int }
    | RedistributeChoices { leftIndex : Int, rightIndex : Int }


shrinkOnce :
    RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( RandomRun, TestingState a )
shrinkOnce counterexample state =
    runShrinkCommands (shrinkCommandsFor counterexample) counterexample state


runShrinkCommand :
    ShrinkCommand
    -> RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( TestingState a, TestCase )
runShrinkCommand cmd randomRun state =
    case cmd of
        DeleteChunk meta ->
            let
                runWithDeletedChunk =
                    RandomRun.deleteChunk meta randomRun
            in
            case runTest (TestCase.forRun runWithDeletedChunk) state of
                Err err ->
                    Err err

                Ok ( nextState, testCase ) ->
                    if
                        not (TestCase.isInteresting testCase)
                            && (meta.startIndex > 0)
                            && (RandomRun.get (meta.startIndex - 1) runWithDeletedChunk /= Just 0)
                    then
                        {- Try reducing the number before this removed chunk,
                           it's frequently the length parameter.
                        -}
                        let
                            runWithDecrementedValue =
                                runWithDeletedChunk
                                    |> RandomRun.update (meta.startIndex - 1) (\x -> x - 1)
                        in
                        runTest (TestCase.forRun runWithDecrementedValue) nextState

                    else
                        Ok ( nextState, testCase )

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
                        binarySearch
                            (\newValue run -> RandomRun.set index newValue run)
                            { low = 0
                            , high = value
                            }
                            randomRun
                            state
                    )
                |> Maybe.withDefault (Ok ( state, testCase ))

        SortChunk meta ->
            runTest
                (TestCase.forRun (RandomRun.sortChunk meta randomRun))
                state

        RedistributeChoices meta ->
            {- First we try swapping them if left > right.

               Then we try to (binary-search) minimize the left while keeping the
               sum constant (so what we subtract from left we add to right).
            -}
            case RandomRun.swapIfOutOfOrder meta randomRun of
                Nothing ->
                    Ok ( state, TestCase.forRun randomRun )

                Just { newRun, newLeft, newRight } ->
                    runTest (TestCase.forRun newRun) state
                        |> Result.andThen
                            (\( state_, testCase ) ->
                                if meta.rightIndex < RandomRun.length newRun && newLeft > 0 then
                                    binarySearch
                                        (\newValue run ->
                                            RandomRun.replace
                                                [ ( meta.leftIndex, newValue )
                                                , ( meta.rightIndex, newRight + newLeft - newValue )
                                                ]
                                                run
                                        )
                                        { low = 0
                                        , high = newLeft
                                        }
                                        newRun
                                        state_

                                else
                                    Ok ( state_, testCase )
                            )


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

                Ok ( nextState, testCase ) ->
                    loopShrink
                        shrinkFn
                        (toLoopState (TestCase.isInteresting testCase))
                        nextRandomRun
                        nextState

        Stop ->
            Ok ( state, TestCase.forRun randomRun )


type alias BinarySearchState =
    { low : Int
    , high : Int
    }


binarySearch :
    (Int -> RandomRun -> RandomRun)
    -> BinarySearchState
    -> RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( TestingState a, TestCase )
binarySearch updateRun binarySearchState run state =
    let
        runWithLow =
            updateRun binarySearchState.low run
    in
    case runTest (TestCase.forRun runWithLow) state of
        Err err ->
            Err err

        Ok ( nextState, testCase ) ->
            if TestCase.isInteresting testCase then
                -- this is the best we could have hoped for
                Ok ( nextState, testCase )

            else
                loopShrink
                    (binarySearchLoop updateRun)
                    binarySearchState
                    run
                    nextState


binarySearchLoop : (Int -> RandomRun -> RandomRun) -> BinarySearchState -> RandomRun -> Loop BinarySearchState
binarySearchLoop updateRun ({ low, high } as state) randomRun =
    if low + 1 < high then
        let
            mid =
                low + (high - low) // 2

            newRandomRun =
                updateRun mid randomRun
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


shrinkCommandsFor : RandomRun -> List ShrinkCommand
shrinkCommandsFor counterexample =
    let
        itemsCount =
            RandomRun.length counterexample
    in
    OurExtras.List.fastConcat
        [ deletionShrinkCommands itemsCount
        , zeroShrinkCommands itemsCount
        , binarySearchShrinkCommands itemsCount
        , sortShrinkCommands itemsCount
        , redistributeShrinkCommands itemsCount
        ]


deletionShrinkCommands : Int -> List ShrinkCommand
deletionShrinkCommands itemsCount =
    blockShrinks
        DeleteChunk
        { itemsCount = itemsCount
        , allowChunksOfSize1 = True
        }


zeroShrinkCommands : Int -> List ShrinkCommand
zeroShrinkCommands itemsCount =
    blockShrinks
        ReplaceChunkWithZero
        { itemsCount = itemsCount
        , allowChunksOfSize1 = False -- already happens in binary search shrinking
        }


binarySearchShrinkCommands : Int -> List ShrinkCommand
binarySearchShrinkCommands itemsCount =
    List.range 0 (itemsCount - 1)
        |> List.reverse
        |> List.map (\index -> MinimizeChoiceWithBinarySearch { index = index })


sortShrinkCommands : Int -> List ShrinkCommand
sortShrinkCommands itemsCount =
    blockShrinks
        SortChunk
        { itemsCount = itemsCount
        , allowChunksOfSize1 = False -- doesn't make sense for sorting
        }


redistributeShrinkCommands : Int -> List ShrinkCommand
redistributeShrinkCommands itemsCount =
    let
        forOffset : Int -> List ShrinkCommand
        forOffset offset =
            if offset >= itemsCount then
                []

            else
                List.range 0 (itemsCount - 1 - offset)
                    |> List.reverse
                    |> List.map
                        (\leftIndex ->
                            RedistributeChoices
                                { leftIndex = leftIndex
                                , rightIndex = leftIndex + offset
                                }
                        )
    in
    forOffset 2 ++ forOffset 1


{-| Not entirely an exact port of the Python Minithesis behaviour,
eg. it doesn't retry deleting at the same index after deleting a chunk.

    blockShrinks
        DeleteChunk
        { itemsCount = 10, allowChunksOfSize1 = False }
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
        ]

-}
blockShrinks :
    ({ chunkSize : Int, startIndex : Int } -> a)
    -> { itemsCount : Int, allowChunksOfSize1 : Bool }
    -> List a
blockShrinks toShrink { itemsCount, allowChunksOfSize1 } =
    let
        initChunkSize : Int
        initChunkSize =
            if allowChunksOfSize1 then
                1

            else
                2

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
    go initChunkSize 0 []
