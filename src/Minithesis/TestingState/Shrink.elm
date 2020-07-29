module Minithesis.TestingState.Shrink exposing
    ( deleteChunkAndMaybeDecrementPrevious
    , minimizeChoiceWithBinarySearch
    , redistributeChoices
    , replaceChunkWithZero
    , sortChunk
    )

import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , TestCase
        )
import Minithesis.TestingState.Internal as Internal exposing (TestingState)



-- Loop abstraction


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
            case Internal.runTest (TestCase.forRun nextRandomRun) state of
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
    case Internal.runTest (TestCase.forRun runWithLow) state of
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



-- Shrinking commands


deleteChunkAndMaybeDecrementPrevious :
    { chunkSize : Int, startIndex : Int }
    -> RandomRun
    -> TestingState a
    -> Maybe ( TestingState a, TestCase )
deleteChunkAndMaybeDecrementPrevious meta randomRun state =
    if meta.startIndex + meta.chunkSize > RandomRun.length randomRun then
        Nothing

    else
        let
            runWithDeletedChunk =
                RandomRun.deleteChunk meta randomRun
        in
        case Internal.runTest (TestCase.forRun runWithDeletedChunk) state of
            Err _ ->
                Nothing

            Ok ( nextState, testCase ) ->
                if TestCase.isInteresting testCase then
                    Just ( nextState, testCase )

                else if
                    (meta.startIndex > 0)
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
                    Internal.runTest (TestCase.forRun runWithDecrementedValue) nextState
                        |> Result.toMaybe

                else
                    Nothing


replaceChunkWithZero :
    { chunkSize : Int, startIndex : Int }
    -> RandomRun
    -> TestingState a
    -> Maybe ( TestingState a, TestCase )
replaceChunkWithZero meta randomRun state =
    if meta.startIndex + meta.chunkSize > RandomRun.length randomRun then
        Nothing

    else
        Internal.runTest (TestCase.forRun (RandomRun.replaceChunkWithZero meta randomRun)) state
            |> Result.toMaybe


minimizeChoiceWithBinarySearch :
    { index : Int }
    -> RandomRun
    -> TestingState a
    -> Maybe ( TestingState a, TestCase )
minimizeChoiceWithBinarySearch { index } randomRun state =
    if index > RandomRun.length randomRun then
        Nothing

    else
        RandomRun.get index randomRun
            |> Maybe.andThen
                (\value ->
                    binarySearch
                        (\newValue run -> RandomRun.set index newValue run)
                        { low = 0
                        , high = value
                        }
                        randomRun
                        state
                        |> Result.toMaybe
                )


sortChunk :
    { chunkSize : Int, startIndex : Int }
    -> RandomRun
    -> TestingState a
    -> Maybe ( TestingState a, TestCase )
sortChunk meta randomRun state =
    if meta.startIndex + meta.chunkSize > RandomRun.length randomRun then
        Nothing

    else
        Internal.runTest (TestCase.forRun (RandomRun.sortChunk meta randomRun)) state
            |> Result.toMaybe


redistributeChoices :
    { leftIndex : Int, rightIndex : Int }
    -> RandomRun
    -> TestingState a
    -> Maybe ( TestingState a, TestCase )
redistributeChoices meta randomRun state =
    let
        length =
            RandomRun.length randomRun
    in
    if meta.leftIndex > length || meta.rightIndex > length then
        Nothing

    else
        {- First we try swapping them if left > right.

           Then we try to (binary-search) minimize the left while keeping the
           sum constant (so what we subtract from left we add to right).
        -}
        case RandomRun.swapIfOutOfOrder meta randomRun of
            Nothing ->
                Nothing

            Just { newRun, newLeft, newRight } ->
                Internal.runTest (TestCase.forRun newRun) state
                    |> Result.toMaybe
                    |> Maybe.andThen
                        (\( state_, _ ) ->
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
                                    |> Result.toMaybe

                            else
                                Nothing
                        )
