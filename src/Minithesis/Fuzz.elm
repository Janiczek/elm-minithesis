module Minithesis.Fuzz exposing
    ( Fuzzer
    , andMap
    , andThen
    , anyInt
    , bool
    , constant
    , filter
    , int
    , list
    , listOfLength
    , listWith
    , map
    , map2
    , map3
    , oneOf
    , oneOfValues
    , reject
    , run
    , tuple
    , tuple3
    , unit
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
                        let
                            newTestCase =
                                { testCase
                                    | seed = newSeed
                                    , randomRun = RandomRun.append result testCase.randomRun
                                }
                        in
                        if result > n then
                            newTestCase
                                |> TestCase.markStatus Invalid

                        else
                            Ok ( result, newTestCase )
                    )



-- 2) BUILDING BLOCKS


{-| Returns a number in the range [0, n] (inclusive).
-}
nonnegativeInt : Int -> Fuzzer Int
nonnegativeInt n =
    Fuzzer (makeChoice n (nonnegativeIntGenerator n))


nonnegativeIntGenerator : Int -> Random.Generator Int
nonnegativeIntGenerator n =
    Random.int 0 n


{-| Returns a Bool, with True having chance `p` [0..1].

Input probabilities outside the [0..1] range will be clamped to [0..1].

-}
weightedBool : Float -> Fuzzer Bool
weightedBool p =
    if p <= 0 then
        forcedChoice 0
            |> map toBool

    else if p >= 1 then
        forcedChoice 1
            |> map toBool

    else
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


forcedChoice : Int -> Fuzzer Int
forcedChoice n =
    Fuzzer
        (\testCase ->
            if n < 0 then
                Err ( InvalidChoice n, testCase )

            else if testCase.status /= Undecided then
                Err ( Frozen, testCase )

            else if RandomRun.length testCase.randomRun >= testCase.maxSize then
                testCase
                    |> TestCase.markStatus Overrun

            else
                Ok
                    ( n
                    , { testCase
                        | randomRun =
                            RandomRun.append n testCase.randomRun
                      }
                    )
        )


toBool : Int -> Bool
toBool int_ =
    case int_ of
        0 ->
            False

        _ ->
            True



-- 3) COMPOSITE FUZZERS


bool : Fuzzer Bool
bool =
    oneOfValues [ True, False ]


{-| Returns a number in the range [from, to] (inclusive).

The range of supported values is (Random.minInt, Random.maxInt):

    MFuzz.int -2147483648 2147483647

-}
int : Int -> Int -> Fuzzer Int
int from to =
    if from > to then
        int to from

    else
        nonnegativeInt (to - from)
            |> map (\n -> n + from)


anyInt : Fuzzer Int
anyInt =
    int Random.minInt Random.maxInt


list : Fuzzer a -> Fuzzer (List a)
list item =
    listWith
        { minLength = Nothing
        , maxLength = Nothing
        }
        item


listOfLength : Int -> Fuzzer a -> Fuzzer (List a)
listOfLength length item =
    listWith
        { minLength = Just length
        , maxLength = Just length
        }
        item


listWith : { minLength : Maybe Int, maxLength : Maybe Int } -> Fuzzer a -> Fuzzer (List a)
listWith { minLength, maxLength } item =
    let
        minLength_ : Int
        minLength_ =
            Maybe.withDefault 0 minLength

        intInfinity : Int
        intInfinity =
            round (1 / 0)

        maxLength_ : Int
        maxLength_ =
            Maybe.withDefault intInfinity maxLength

        addItem : Int -> List a -> Fuzzer (List a)
        addItem accLength accList =
            item |> andThen (\x -> go (accLength + 1) (x :: accList))

        go : Int -> List a -> Fuzzer (List a)
        go accLength accList =
            if accLength < minLength_ then
                forcedChoice 1
                    |> andThen (\_ -> addItem accLength accList)

            else if accLength + 1 >= maxLength_ then
                forcedChoice 0
                    |> andThen (\_ -> constant (List.reverse accList))

            else
                weightedBool 0.9
                    |> andThen
                        (\coin ->
                            if coin then
                                addItem accLength accList

                            else
                                constant (List.reverse accList)
                        )
    in
    go 0 []


tuple : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
tuple ( a, b ) =
    map2 Tuple.pair a b


tuple3 : ( Fuzzer a, Fuzzer b, Fuzzer c ) -> Fuzzer ( a, b, c )
tuple3 ( a, b, c ) =
    map3 (\ax bx cx -> ( ax, bx, cx )) a b c


constant : a -> Fuzzer a
constant a =
    Fuzzer (\testCase -> Ok ( a, testCase ))


oneOfValues : List a -> Fuzzer a
oneOfValues constants =
    case List.length constants of
        0 ->
            reject

        length ->
            nonnegativeInt (length - 1)
                |> andThen
                    (\chosenValueIndex ->
                        case constants |> List.drop chosenValueIndex |> List.head of
                            Nothing ->
                                -- shouldn't happen
                                reject

                            Just chosenValue ->
                                constant chosenValue
                    )


oneOf : List (Fuzzer a) -> Fuzzer a
oneOf fuzzers =
    case List.length fuzzers of
        0 ->
            reject

        length ->
            nonnegativeInt (length - 1)
                |> andThen
                    (\chosenFuzzerIndex ->
                        case fuzzers |> List.drop chosenFuzzerIndex |> List.head of
                            Nothing ->
                                -- shouldn't happen
                                reject

                            Just chosenFuzzer ->
                                chosenFuzzer
                    )


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


map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 fn a b c =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c


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


reject : Fuzzer a
reject =
    Fuzzer (TestCase.markStatus Invalid)


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


unit : Fuzzer ()
unit =
    constant ()
