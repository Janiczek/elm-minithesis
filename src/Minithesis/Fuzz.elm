module Minithesis.Fuzz exposing
    ( Fuzzer
    , andMap
    , andThen
    , bool
    , constant
    , filter
    , map
    , map2
    , nonnegativeInt
    , nonnegativeIntFromTo
    , run
    , weightedBool
    )

import Minithesis.RandomRun as RandomRun
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , TestCase
        )
import Random



-- 1) UNDERLYING ABSTRACTION


type Fuzzer a
    = Fuzzer (TestCase -> Result ( Stop, TestCase ) ( a, TestCase ))


run : Fuzzer a -> TestCase -> Result ( Stop, TestCase ) ( a, TestCase )
run (Fuzzer fn) testCase =
    fn testCase


{-| All fuzzers need to somehow go through picking an Int.
(Sequences of Ints are what the shrinkers work on, they're the underlying
abstraction (see the RandomRun module).

This function makes a choice in [0, n], by using the generator if randomness is
needed (or the prefix if some choices are predetermined).

-}
makeChoice :
    Int
    -> Random.Generator Int
    -> TestCase
    -> Result ( Stop, TestCase ) ( Int, TestCase )
makeChoice n generator testCase =
    -- TODO not caring about n >= 2^64 or any such limit
    if n < 0 then
        Err ( InvalidChoice n, testCase )

    else if testCase.status /= Undecided then
        Err ( Frozen, testCase )

    else
        let
            runLength =
                RandomRun.length testCase.randomRun

            prefixLength =
                RandomRun.length testCase.prefix
        in
        if runLength >= testCase.maxSize then
            testCase
                |> TestCase.markStatus Overrun

        else
            let
                resultAndNewSeed : Result Stop ( Int, Maybe Random.Seed )
                resultAndNewSeed =
                    if runLength < prefixLength then
                        RandomRun.get runLength testCase.prefix
                            |> Maybe.map
                                (\prefixChoice ->
                                    ( prefixChoice
                                    , testCase.seed
                                    )
                                )
                            |> Result.fromMaybe PrefixNotLongEnough

                    else
                        testCase.seed
                            |> Maybe.map
                                (Random.step generator
                                    >> Tuple.mapSecond Just
                                )
                            |> Result.fromMaybe RandomnessNotAllowed
            in
            resultAndNewSeed
                |> Result.mapError (\stop -> ( stop, testCase ))
                |> Result.andThen
                    (\( result, newSeed ) ->
                        if result > n then
                            { testCase | seed = newSeed }
                                |> TestCase.markStatus Invalid

                        else
                            Ok
                                ( result
                                , { testCase
                                    | seed = newSeed
                                    , randomRun = RandomRun.append result testCase.randomRun
                                  }
                                )
                    )



-- 2) BUILDING BLOCKS


{-| Returns a number in the range [0, n]
-}
nonnegativeInt : Int -> Fuzzer Int
nonnegativeInt n =
    Fuzzer (makeChoice n (nonnegativeIntGenerator n))


nonnegativeIntGenerator : Int -> Random.Generator Int
nonnegativeIntGenerator n =
    Random.int 0 n


{-| Returns a Bool, with True having chance `p` (0..1)
-}
weightedBool : Float -> Fuzzer Bool
weightedBool p =
    Fuzzer
        (\testCase ->
            makeChoice 1 (weightedBoolGenerator p) testCase
                |> Result.map (Tuple.mapFirst toBool)
        )


weightedBoolGenerator : Float -> Random.Generator Int
weightedBoolGenerator p =
    Random.float 0 1
        |> Random.map
            (\f ->
                if f <= p then
                    1

                else
                    0
            )


toBool : Int -> Bool
toBool int =
    case int of
        0 ->
            False

        _ ->
            True



-- 3) COMPOSITE FUZZERS


bool : Fuzzer Bool
bool =
    weightedBool 0.5


nonnegativeIntFromTo : Int -> Int -> Fuzzer Int
nonnegativeIntFromTo from to =
    if from > to then
        nonnegativeIntFromTo to from

    else
        nonnegativeInt (to - from)
            |> map (\n -> n + from)



-- 4) HELPERS


constant : a -> Fuzzer a
constant a =
    Fuzzer (\testCase -> Ok ( a, testCase ))


map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn (Fuzzer fuzzer) =
    Fuzzer
        (\testCase ->
            fuzzer testCase
                |> Result.map (Tuple.mapFirst fn)
        )


map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) =
    Fuzzer
        (\testCase0 ->
            fuzzerA testCase0
                |> Result.andThen
                    (\( a, testCase1 ) ->
                        fuzzerB testCase1
                            |> Result.map (Tuple.mapFirst (fn a))
                    )
        )


andMap : Fuzzer a -> Fuzzer (a -> b) -> Fuzzer b
andMap =
    map2 (|>)


andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b
andThen fn (Fuzzer fuzzer) =
    Fuzzer
        (\testCase ->
            fuzzer testCase
                |> Result.andThen
                    (\( value, newTestCase ) ->
                        let
                            (Fuzzer newFuzzer) =
                                fn value
                        in
                        newFuzzer newTestCase
                    )
        )


filter : (a -> Bool) -> Fuzzer a -> Fuzzer a
filter fn (Fuzzer fuzzer) =
    Fuzzer
        (\testCase ->
            fuzzer testCase
                |> Result.andThen
                    (\( value, newTestCase ) ->
                        if fn value then
                            Ok ( value, newTestCase )

                        else
                            newTestCase
                                |> TestCase.markStatus Invalid
                    )
        )
