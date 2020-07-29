module Minithesis.TestingState exposing
    ( Test(..)
    , generate
    , init
    , shrink
    , stopIfUnsatisfiable
    )

import Minithesis.Fuzz.Internal as Fuzz exposing (Fuzzer)
import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , TestCase
        )
import Minithesis.TestingState.Internal as Internal
    exposing
        ( ShrinkCommand(..)
        , TestingState
        )
import Minithesis.TestingState.Shrink as Shrink
import OurExtras.List
import Random


type Test a
    = Test
        { label : String
        , userTestFn : TestCase -> Result ( Stop, TestCase ) ( Bool, TestCase )
        , fuzzer : Fuzzer a
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
    -> Bool
    -> Test a
    -> TestingState a
init seed maxExamples showShrinkHistory (Test { label, userTestFn, fuzzer }) =
    { seed = seed
    , maxExamples = maxExamples
    , showShrinkHistory = showShrinkHistory
    , label = label
    , userTestFn = markFailuresInteresting userTestFn
    , fuzzer = fuzzer
    , validTestCases = 0
    , calls = 0
    , bestCounterexample = Nothing
    , previousBestCounterexample = Nothing
    , shrinkHistory = []
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
                    (Internal.runTest
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


stopIfUnsatisfiable :
    Result ( Stop, TestCase ) (TestingState a)
    -> Result ( Stop, TestCase ) (TestingState a)
stopIfUnsatisfiable result =
    case result of
        Err _ ->
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
                        state
                            |> addShrinkToHistory Nothing counterexample
                            |> iterateShrinkWhileProgress counterexample
                            |> Ok

        Err _ ->
            result


addShrinkToHistory : Maybe ShrinkCommand -> RandomRun -> TestingState a -> TestingState a
addShrinkToHistory cmd counterexample state =
    if state.showShrinkHistory then
        if state.previousBestCounterexample == Just counterexample then
            state

        else
            case Fuzz.run state.fuzzer (TestCase.forRun counterexample) of
                Ok ( value, _ ) ->
                    { state
                        | shrinkHistory = ( value, counterexample, cmd ) :: state.shrinkHistory
                        , previousBestCounterexample = Just counterexample
                    }

                Err _ ->
                    -- shouldn't happen
                    state

    else
        state


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


shrinkOnce :
    RandomRun
    -> TestingState a
    -> Result ( Stop, TestCase ) ( RandomRun, TestingState a )
shrinkOnce counterexample state =
    runShrinkCommands (shrinkCommandsFor counterexample) counterexample state


{-| Fail if can't shrink successfully. This will be picked upon in
runShrinkCommands; it won't halt the whole thing.
-}
runShrinkCommand :
    ShrinkCommand
    -> RandomRun
    -> TestingState a
    -> Maybe ( TestingState a, TestCase )
runShrinkCommand cmd randomRun state =
    case cmd of
        DeleteChunkAndMaybeDecrementPrevious meta ->
            Shrink.deleteChunkAndMaybeDecrementPrevious meta randomRun state

        ReplaceChunkWithZero meta ->
            Shrink.replaceChunkWithZero meta randomRun state

        MinimizeChoiceWithBinarySearch meta ->
            Shrink.minimizeChoiceWithBinarySearch meta randomRun state

        SortChunk meta ->
            Shrink.sortChunk meta randomRun state

        RedistributeChoices meta ->
            Shrink.redistributeChoices meta randomRun state


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
                    case runShrinkCommand cmd accRandomRun accState of
                        Nothing ->
                            {- Shrink command was unsuccessful. Keep on going, just ignore this one!
                               As shrinks don't advance the seeds, there's nothing to salvage here.
                            -}
                            result

                        Just ( newState, testCase ) ->
                            -- That command was successful, great, continue!
                            case newState.bestCounterexample of
                                Nothing ->
                                    -- shouldn't happen
                                    Err ( LostCounterexample, testCase )

                                Just newChoice ->
                                    Ok
                                        ( newChoice
                                        , newState
                                            |> addShrinkToHistory (Just cmd) newChoice
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
        DeleteChunkAndMaybeDecrementPrevious
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
        SortChunk
        { itemsCount = 10, allowChunksOfSize1 = False }
        -->
        [ -- Chunks of size 8
          SortChunk { chunkSize = 8, startIndex = 2 }
        , SortChunk { chunkSize = 8, startIndex = 1 }
        , SortChunk { chunkSize = 8, startIndex = 0 }

        -- Chunks of size 4
        , SortChunk { chunkSize = 4, startIndex = 6 }
        , SortChunk { chunkSize = 4, startIndex = 5 }
        , -- ...
          SortChunk { chunkSize = 4, startIndex = 1 }
        , SortChunk { chunkSize = 4, startIndex = 0 }

        -- Chunks of size 2
        , SortChunk { chunkSize = 2, startIndex = 8 }
        , SortChunk { chunkSize = 2, startIndex = 7 }
        , -- ...
          SortChunk { chunkSize = 2, startIndex = 1 }
        , SortChunk { chunkSize = 2, startIndex = 0 }
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
