module Minithesis.Fuzz exposing
    ( Fuzzer, example, exampleWithSeed
    , bool, weightedBool
    , int, anyNumericInt, anyInt, positiveInt, negativeInt, nonpositiveInt, nonnegativeInt
    , float, anyNumericFloat, anyFloat, floatWith, probability
    , char, charRange, anyChar
    , string, stringOfLength, stringWith
    , unit
    , maybe, result
    , tuple, tuple3
    , list, listOfLength, listWith
    , uniqueList, uniqueListOfLength, uniqueListWith
    , uniqueByList, uniqueByListOfLength, uniqueByListWith
    , map, andMap, map2, map3, map4, map5, map6, map7, map8
    , andThen, constant, reject, filter
    , oneOf, oneOfValues, frequency, frequencyValues
    {- , exampleRun -}
    )

{-|


# The basics

@docs Fuzzer, example, exampleWithSeed, exampleRun


# Values

@docs bool, weightedBool

@docs int, anyNumericInt, anyInt, positiveInt, negativeInt, nonpositiveInt, nonnegativeInt

@docs float, anyNumericFloat, anyFloat, floatWith, probability

@docs char, charRange, anyChar

@docs string, stringOfLength, stringWith

@docs unit


# Collections

@docs maybe, result

@docs tuple, tuple3

@docs list, listOfLength, listWith

@docs uniqueList, uniqueListOfLength, uniqueListWith

@docs uniqueByList, uniqueByListOfLength, uniqueByListWith


# Combinators

@docs map, andMap, map2, map3, map4, map5, map6, map7, map8

@docs andThen, constant, reject, filter

@docs oneOf, oneOfValues, frequency, frequencyValues

-}

import Char exposing (Char)
import List.Extra
import Minithesis.Fuzz.Float as Float
import Minithesis.Fuzz.Internal as Internal exposing (Fuzzer(..))
import Minithesis.RandomRun as RandomRun
import Minithesis.Stop exposing (Stop(..))
import Minithesis.TestCase as TestCase
    exposing
        ( Status(..)
        , TestCase
        )
import Random
import Set exposing (Set)



-- 1) UNDERLYING ABSTRACTION


type alias Fuzzer a =
    Internal.Fuzzer a


example : Fuzzer a -> List a
example fuzzer =
    exampleWithSeed 0 fuzzer


exampleWithSeed : Int -> Fuzzer a -> List a
exampleWithSeed seedInt (Fuzzer fn) =
    let
        fallbackSeed : Random.Seed -> Random.Seed
        fallbackSeed seed =
            seed
                |> Random.step (Random.constant ())
                |> Tuple.second

        nextSeed : Random.Seed -> Maybe Random.Seed -> Random.Seed
        nextSeed previousSeed maybeNextSeed =
            case maybeNextSeed of
                Just seed ->
                    seed

                Nothing ->
                    fallbackSeed previousSeed

        go : Int -> Random.Seed -> List a -> List a
        go i seed acc =
            if i <= 0 then
                acc

            else
                case
                    fn
                        (TestCase.init
                            { seed = seed

                            -- TODO perhaps we can make this a Maybe so that this never hangs?
                            , maxSize = 1000
                            , prefix = RandomRun.empty
                            }
                        )
                of
                    Ok ( value, testCase ) ->
                        go
                            (i - 1)
                            (nextSeed seed testCase.seed)
                            (value :: acc)

                    Err ( _, testCase ) ->
                        go
                            i
                            (nextSeed seed testCase.seed)
                            acc
    in
    go 10 (Random.initialSeed seedInt) []



--exampleRun : Fuzzer a -> Result Stop (a, List Int)
--exampleRun (Fuzzer fn) =
--    let
--        fallbackSeed : Random.Seed -> Random.Seed
--        fallbackSeed seed =
--            seed
--                |> Random.step (Random.constant ())
--                |> Tuple.second
--
--        nextSeed : Random.Seed -> Maybe Random.Seed -> Random.Seed
--        nextSeed previousSeed maybeNextSeed =
--            case maybeNextSeed of
--                Just seed ->
--                    seed
--
--                Nothing ->
--                    fallbackSeed previousSeed
--
--        go : Int -> Random.Seed -> List a -> List a
--        go i seed acc =
--            if i <= 0 then
--                acc
--
--            else
--                case
--                    fn
--                        (TestCase.init
--                            { seed = seed
--
--                            -- TODO perhaps we can make this a Maybe so that this never hangs?
--                            , maxSize = 1000
--                            , prefix = RandomRun.empty
--                            }
--                        )
--                of
--                    Ok ( value, testCase ) ->
--                        go
--                            (i - 1)
--                            (nextSeed seed testCase.seed)
--                            (value :: acc)
--
--                    Err ( _, testCase ) ->
--                        go
--                            i
--                            (nextSeed seed testCase.seed)
--                            acc
--    in
--    go 10 (Random.initialSeed seedInt) []


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
            runLength : Int
            runLength =
                RandomRun.length testCase.randomRun
        in
        if runLength >= testCase.maxSize then
            testCase
                |> TestCase.markStatus Overrun

        else
            let
                prefixLength : Int
                prefixLength =
                    RandomRun.length testCase.prefix

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
                    (\( result_, newSeed ) ->
                        let
                            newTestCase =
                                { testCase
                                    | seed = newSeed
                                    , randomRun = RandomRun.append result_ testCase.randomRun
                                }
                        in
                        if result_ > n then
                            newTestCase
                                |> TestCase.markStatus Invalid

                        else
                            Ok ( result_, newTestCase )
                    )



-- 2) BUILDING BLOCKS


{-| Returns a number in the range [0, n] (inclusive).
-}
nonnegativeInt_ : Int -> Fuzzer Int
nonnegativeInt_ n =
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
        reject

    else
        nonnegativeInt_ (to - from)
            |> map (\n -> n + from)


{-| Ranges over all possible integers: [-2147483648, 2147483647]
-}
anyNumericInt : Fuzzer Int
anyNumericInt =
    int Random.minInt Random.maxInt


positiveInt : Fuzzer Int
positiveInt =
    int 1 Random.maxInt


negativeInt : Fuzzer Int
negativeInt =
    int Random.minInt -1


nonpositiveInt : Fuzzer Int
nonpositiveInt =
    int Random.minInt 0


nonnegativeInt : Fuzzer Int
nonnegativeInt =
    int 0 Random.maxInt


intInfinity : Int
intInfinity =
    round (1 / 0)


{-| Ranges over all possible integers: [-2147483648, 2147483647]
and also the Int variants of +Infinity, -Infinity and NaN.
-}
anyInt : Fuzzer Int
anyInt =
    let
        intNaN : Int
        intNaN =
            round (0 / 0)
    in
    frequency
        [ ( 10, anyNumericInt )
        , ( 1, constant intInfinity )
        , ( 1, constant (negate intInfinity) )
        , ( 1, constant intNaN )
        ]


int32 : Fuzzer Int
int32 =
    int 0 0xFFFFFFFF


avg : Float -> Float -> Float
avg x y =
    if isInfinite x || isInfinite y then
        -- TODO maybe deal with the sign? Probably not needed
        1 / 0

    else
        (x + y) / 2


convertIntRange :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Int
    }
    ->
        { minLength : Int
        , maxLength : Int
        , continueProbability : Float
        }
convertIntRange { minLength, maxLength, customAverageLength } =
    let
        min_ =
            minLength
                |> Maybe.withDefault 0
                |> max 0

        max_ =
            maxLength
                |> Maybe.withDefault intInfinity
                |> max 0

        min__ =
            toFloat min_

        max__ =
            toFloat max_

        average =
            case customAverageLength of
                Just avg_ ->
                    toFloat avg_

                Nothing ->
                    -- Taken from Python Hypothesis (ListStrategy)
                    -- This deals with the cases where max is Infinity
                    min
                        (max (min__ * 2) (min__ + 5))
                        (avg min__ max__)

        continueProbability =
            1 - 1 / (1 + average)
    in
    { minLength = min_
    , maxLength = max_
    , continueProbability = continueProbability
    }


list : Fuzzer a -> Fuzzer (List a)
list item =
    listWith
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Nothing
        }
        item


listOfLength : Int -> Fuzzer a -> Fuzzer (List a)
listOfLength length item =
    listWith
        { minLength = Just length
        , maxLength = Just length
        , customAverageLength = Just length
        }
        item


listWith :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Int
    }
    -> Fuzzer a
    -> Fuzzer (List a)
listWith range itemFuzzer =
    let
        { minLength, maxLength, continueProbability } =
            convertIntRange range
    in
    if minLength > maxLength then
        reject

    else if maxLength <= 0 then
        constant []

    else
        let
            addItem : Int -> List a -> Fuzzer (List a)
            addItem accLength accList =
                itemFuzzer
                    |> andThen
                        (\item ->
                            go (accLength + 1) (item :: accList)
                        )

            go : Int -> List a -> Fuzzer (List a)
            go length acc =
                if length < minLength then
                    forcedChoice 1
                        |> andThen (\_ -> addItem length acc)

                else if length == maxLength then
                    forcedChoice 0
                        |> andThen (\_ -> constant (List.reverse acc))

                else
                    weightedBool continueProbability
                        |> andThen
                            (\coin ->
                                if coin then
                                    addItem length acc

                                else
                                    constant (List.reverse acc)
                            )
        in
        go 0 []


uniqueList : Fuzzer comparable -> Fuzzer (List comparable)
uniqueList item =
    uniqueListWith
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Nothing
        }
        item


uniqueListOfLength : Int -> Fuzzer comparable -> Fuzzer (List comparable)
uniqueListOfLength length item =
    uniqueListWith
        { minLength = Just length
        , maxLength = Just length
        , customAverageLength = Just length
        }
        item


uniqueListWith :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Int
    }
    -> Fuzzer comparable
    -> Fuzzer (List comparable)
uniqueListWith range item =
    uniqueByListWith
        identity
        range
        item


uniqueByList : (a -> comparable) -> Fuzzer a -> Fuzzer (List a)
uniqueByList toComparable item =
    uniqueByListWith
        toComparable
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Nothing
        }
        item


uniqueByListOfLength : Int -> (a -> comparable) -> Fuzzer a -> Fuzzer (List a)
uniqueByListOfLength length toComparable item =
    uniqueByListWith
        toComparable
        { minLength = Just length
        , maxLength = Just length
        , customAverageLength = Just length
        }
        item


uniqueByListWith :
    (a -> comparable)
    ->
        { minLength : Maybe Int
        , maxLength : Maybe Int
        , customAverageLength : Maybe Int
        }
    -> Fuzzer a
    -> Fuzzer (List a)
uniqueByListWith toComparable range itemFuzzer =
    let
        { minLength, maxLength, continueProbability } =
            convertIntRange range
    in
    if minLength > maxLength then
        reject

    else if maxLength <= 0 then
        constant []

    else
        let
            addItem : Set comparable -> Int -> List a -> Fuzzer (List a)
            addItem seen length acc =
                itemFuzzer
                    |> filter (\item -> not <| Set.member (toComparable item) seen)
                    |> andThen
                        (\item ->
                            go
                                (Set.insert (toComparable item) seen)
                                (length + 1)
                                (item :: acc)
                        )

            go : Set comparable -> Int -> List a -> Fuzzer (List a)
            go seen length acc =
                if length < minLength then
                    forcedChoice 1
                        |> andThen (\_ -> addItem seen length acc)

                else if length == maxLength then
                    forcedChoice 0
                        |> andThen (\_ -> constant (List.reverse acc))

                else
                    weightedBool continueProbability
                        |> andThen
                            (\coin ->
                                if coin then
                                    addItem seen length acc

                                else
                                    constant (List.reverse acc)
                            )
        in
        go Set.empty 0 []


tuple : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
tuple a b =
    map2 Tuple.pair a b


tuple3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
tuple3 a b c =
    map3 (\ax bx cx -> ( ax, bx, cx )) a b c


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


map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 fn a b c =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c


map4 : (a -> b -> c -> d -> e) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e
map4 fn a b c d =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d


map5 : (a -> b -> c -> d -> e -> f) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f
map5 fn a b c d e =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


map6 : (a -> b -> c -> d -> e -> f -> g) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g
map6 fn a b c d e f =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f


map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g -> Fuzzer h
map7 fn a b c d e f g =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f
        |> andMap g


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g -> Fuzzer h -> Fuzzer i
map8 fn a b c d e f g h =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f
        |> andMap g
        |> andMap h


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
filter fn fuzzer =
    {- TODO Hypothesis tries three times - and what then? Does it sidestep the
       TestingState counters for when to stop generating/shrinking?
    -}
    fuzzer
        |> andThen
            (\item ->
                if fn item then
                    constant item

                else
                    reject
            )


unit : Fuzzer ()
unit =
    constant ()


{-| A fuzzer for Char values. Generates random ASCII chars disregarding the control
characters and the extended character set.

The range used for the char codes is 32 to 126.

-}
char : Fuzzer Char
char =
    charRange 32 126


{-| A fuzzer for Char values. Generates random Unicode characters (even
surrogate pairs).

The range used for the char codes is 0 to 1114111 (0x10FFFF).

-}
anyChar : Fuzzer Char
anyChar =
    charRange 0 maxChar


maxChar : Int
maxChar =
    0x0010FFFF


{-| Use your own char code range for Char generation!

    charRange 48 57 -- will generate chars from '0' to '9'

    charRange 65 90 -- will generate chars from 'A' to 'Z'

    charRange 97 122 -- will generate chars from 'a' to 'z'

-}
charRange : Int -> Int -> Fuzzer Char
charRange from to =
    if from < 0 || to < 0 || from > to || from > maxChar || to > maxChar then
        reject

    else if from == to then
        constant (Char.fromCode from)

    else
        int from to
            |> filter (not << isSurrogate)
            |> map Char.fromCode


isSurrogate : Int -> Bool
isSurrogate charCode =
    charCode >= 0xD800 && charCode <= 0xDFFF


{-| Generates random printable ASCII strings of up to 1000 characters.

Shorter strings are more common, especially the empty string.

-}
string : Fuzzer String
string =
    list char
        |> map String.fromList


stringOfLength : Int -> Fuzzer String
stringOfLength length =
    listOfLength length char
        |> map String.fromList


stringWith :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , charFuzzer : Fuzzer Char
    }
    -> Fuzzer String
stringWith { minLength, maxLength, charFuzzer } =
    listWith
        { minLength = minLength
        , maxLength = maxLength
        , customAverageLength = Nothing
        }
        charFuzzer
        |> map String.fromList


oneOf : List (Fuzzer a) -> Fuzzer a
oneOf fuzzers =
    case List.length fuzzers of
        0 ->
            reject

        length ->
            int 0 (length - 1)
                |> andThen
                    (\i ->
                        case List.head (List.drop i fuzzers) of
                            Nothing ->
                                -- shouldn't be possible
                                reject

                            Just fuzzer ->
                                fuzzer
                    )


oneOfValues : List a -> Fuzzer a
oneOfValues items =
    case List.length items of
        0 ->
            reject

        length ->
            int 0 (length - 1)
                |> andThen
                    (\i ->
                        case List.head (List.drop i items) of
                            Nothing ->
                                -- shouldn't be possible
                                reject

                            Just item ->
                                constant item
                    )


frequency : List ( Float, Fuzzer a ) -> Fuzzer a
frequency options =
    let
        cleanOptions : List ( Float, Fuzzer a )
        cleanOptions =
            options
                |> List.filter (\( weight, _ ) -> weight > 0)
    in
    case List.length cleanOptions of
        0 ->
            reject

        n ->
            let
                ( reverseCumulativeOptions, sum ) =
                    List.foldl
                        (\( weight, fuzzer ) ( accCumulativeOptions, accSum ) ->
                            let
                                newSum =
                                    accSum + weight
                            in
                            ( ( newSum, fuzzer ) :: accCumulativeOptions
                            , newSum
                            )
                        )
                        ( [], 0 )
                        cleanOptions

                cumulativeOptions =
                    List.reverse reverseCumulativeOptions
            in
            probability
                |> andThen
                    (\p ->
                        let
                            f =
                                p * sum
                        in
                        case List.Extra.find (\( weight, _ ) -> weight >= f) cumulativeOptions of
                            Nothing ->
                                reject

                            Just ( _, fuzzer ) ->
                                fuzzer
                    )


frequencyValues : List ( Float, a ) -> Fuzzer a
frequencyValues options =
    frequency (List.map (Tuple.mapSecond constant) options)


maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe item =
    -- The order here is important: we shrink to the items earlier in the list
    oneOf
        [ constant Nothing
        , map Just item
        ]


result : Fuzzer x -> Fuzzer a -> Fuzzer (Result x a)
result errFuzzer okFuzzer =
    oneOf
        [ map Err errFuzzer
        , map Ok okFuzzer
        ]


{-| Returns Float in range 0..1 inclusive (uniform distribution).
-}
probability : Fuzzer Float
probability =
    tuple int32 int32
        |> map Float.fractionalFloat


float : Float -> Float -> Fuzzer Float
float from to =
    floatWith
        { min = Just from
        , max = Just to
        , allowNaN = False
        , allowInfinities = False
        }


{-| Ranges over all possible floats: [-1.7976931348623157e308, 1.7976931348623157e308]
except for +Infinity, -Infinity and NaN.
-}
anyNumericFloat : Fuzzer Float
anyNumericFloat =
    anyFloatWith
        { allowNaN = False
        , allowInfinities = False
        }


{-| Ranges over all possible floats: [-1.7976931348623157e308, 1.7976931348623157e308]
and also the +Infinity, -Infinity and NaN.
-}
anyFloat : Fuzzer Float
anyFloat =
    anyFloatWith
        { allowNaN = True
        , allowInfinities = True
        }


anyFloatWith : { allowNaN : Bool, allowInfinities : Bool } -> Fuzzer Float
anyFloatWith { allowNaN, allowInfinities } =
    let
        isPermitted : Float -> Bool
        isPermitted f =
            if isNaN f then
                allowNaN

            else if isInfinite f then
                allowInfinities

            else
                True

        nastyFloats : List Float
        nastyFloats =
            List.filter isPermitted Float.nastyFloats
    in
    frequency
        [ ( 0.5, wellShrinkingFloat { allowInfinities = allowInfinities } )
        , ( 0.5, oneOfValues nastyFloats )
        ]


{-| (Cannot generate NaNs.)
-}
wellShrinkingFloat : { allowInfinities : Bool } -> Fuzzer Float
wellShrinkingFloat { allowInfinities } =
    map3
        (\highBits lowBits shouldNegate ->
            let
                f : Float
                f =
                    Float.lexToFloat ( highBits, lowBits )
            in
            if shouldNegate then
                negate f

            else
                f
        )
        int32
        int32
        bool
        |> (if allowInfinities then
                identity

            else
                filter (not << isInfinite)
           )


floatWith :
    { min : Maybe Float
    , max : Maybe Float
    , allowNaN : Bool
    , allowInfinities : Bool
    }
    -> Fuzzer Float
floatWith ({ min, max, allowNaN, allowInfinities } as options) =
    {- TODO if we figure out how to do nextUp and nextDown for IEEE 734 floats,
       we'll be able to do exclodeMin : Bool, excludeMax : Bool
    -}
    case ( min, max ) of
        ( Nothing, Nothing ) ->
            anyFloatWith
                { allowNaN = allowNaN
                , allowInfinities = allowInfinities
                }

        ( Just min_, Nothing ) ->
            if min_ < 0 then
                oneOf
                    [ map abs anyNumericFloat
                    , floatWith
                        { min = Just min_
                        , max = Just -0.0
                        , allowNaN = allowNaN
                        , allowInfinities = allowInfinities
                        }
                    ]

            else
                map (\f -> min_ + abs f) anyNumericFloat

        ( Nothing, Just max_ ) ->
            if max_ >= 0 then
                oneOf
                    [ floatWith
                        { min = Just 0.0
                        , max = Just max_
                        , allowNaN = allowNaN
                        , allowInfinities = allowInfinities
                        }
                    , map (abs >> negate) anyNumericFloat
                    ]

            else
                map (\f -> max_ - abs f) anyNumericFloat

        ( Just min_, Just max_ ) ->
            if min_ > max_ then
                reject

            else if isNaN min_ || isNaN max_ || isInfinite min_ || isInfinite max_ then
                reject

            else if min_ == max_ then
                constant min_

            else
                probability
                    |> map (\f -> f * (max_ - min_) + min_)
