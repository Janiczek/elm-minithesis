module Minithesis.Fuzz exposing
    ( Fuzzer, example, exampleWithSeed, generate, generateWithSeed
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
    , lazy
    , oneOf, oneOfValues, frequency, frequencyValues
    )

{-|


# The basics

@docs Fuzzer, example, exampleWithSeed, generate, generateWithSeed


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

@docs lazy

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



{- TODO more whitespace in string/char fuzzer?
   TODO ints shrinking toward zero?
   TODO negative floats shrinking to positive floats?
-}
-- 1) UNDERLYING ABSTRACTION


{-| `Fuzzer` is a recipe for generating a value.

Minithesis fuzzers differ from `elm-test` ones in that they internally remember
the random numbers drawn from the PRNG; this allows us to later shrink those
random numbers and try generating a new value from them.

-}
type alias Fuzzer a =
    Internal.Fuzzer a


{-| Shows 10 examples of randomly generated values from the given fuzzer, with
0 as the PRNG seed.

     Fuzz.example (Fuzz.uniqueList (Fuzz.int 1 3))
     -->
     [[1],[3],[],[3],[],[],[3],[3,2],[3],[1,3,2]]

-}
example : Fuzzer a -> List a
example fuzzer =
    exampleWithSeed 0 fuzzer


{-| Shows 10 examples of randomly generated values from the given fuzzer, with
the given PRNG seed.

Useful for quick sanity checks in the REPL:

     Fuzz.uniqueList (Fuzz.int 1 3) |> Fuzz.exampleWithSeed 0
     -->
     [[1],[3],[],[3],[],[],[3],[3,2],[3],[1,3,2]]


     Fuzz.uniqueList (Fuzz.int 1 3) |> Fuzz.exampleWithSeed 1
     -->
     [[1,3],[3,2],[],[],[],[1],[],[3],[3,1],[2]]


     Fuzz.uniqueList (Fuzz.int 1 3) |> Fuzz.exampleWithSeed 2
     -->
     [[],[1],[2,1],[],[2],[1],[1,2],[3,1],[3,1],[]]

-}
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

        go : Int -> Int -> Random.Seed -> List a -> List a
        go tries i seed acc =
            if tries <= 0 then
                -- unsuccessful, perhaps it rejects too much
                go
                    100
                    (i - 1)
                    seed
                    acc

            else if i <= 0 then
                acc

            else
                case
                    fn
                        (TestCase.init
                            { seed = seed
                            , maxSize = 100
                            , prefix = RandomRun.empty
                            }
                        )
                of
                    Ok ( value, testCase ) ->
                        go
                            100
                            (i - 1)
                            (nextSeed seed testCase.seed)
                            (value :: acc)

                    Err ( _, testCase ) ->
                        go
                            (tries - 1)
                            i
                            (nextSeed seed testCase.seed)
                            acc
    in
    go 100 10 (Random.initialSeed seedInt) []


{-| Make the fuzzer generate an example.

It will start with the seed 0, try 100 times and then give up (so that eg.
`Fuzz.generate Fuzz.reject` won't freeze your computer).

-}
generate : Fuzzer a -> Maybe a
generate ((Fuzzer fuzzer) as wrappedFuzzer) =
    let
        fallbackSeed : Random.Seed -> Random.Seed
        fallbackSeed seed =
            seed
                |> Random.step (Random.constant ())
                |> Tuple.second

        go : Int -> Random.Seed -> Maybe a
        go tries seed =
            if tries <= 0 then
                Nothing

            else
                let
                    result_ =
                        fuzzer
                            (TestCase.init
                                { seed = seed
                                , maxSize = TestCase.defaultBufferSize
                                , prefix = RandomRun.empty
                                }
                            )
                in
                case result_ of
                    Ok ( value, _ ) ->
                        Just value

                    Err ( _, testCase ) ->
                        go
                            (tries - 1)
                            (case testCase.seed of
                                Nothing ->
                                    fallbackSeed seed

                                Just seed_ ->
                                    seed_
                            )
    in
    go 100 (Random.initialSeed 0)


{-| Make the fuzzer generate an example with the given `Random.Seed`.

It will start with the given `Random.Seed`, try 100 times and then give up (so
that eg. `Fuzz.generate Fuzz.reject` won't freeze your computer).

-}
generateWithSeed : Random.Seed -> Fuzzer a -> Maybe ( a, Random.Seed )
generateWithSeed initSeed ((Fuzzer fuzzer) as wrappedFuzzer) =
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

        go : Int -> Random.Seed -> Maybe ( a, Random.Seed )
        go tries seed =
            if tries <= 0 then
                Nothing

            else
                let
                    result_ =
                        fuzzer
                            (TestCase.init
                                { seed = seed
                                , maxSize = TestCase.defaultBufferSize
                                , prefix = RandomRun.empty
                                }
                            )
                in
                case result_ of
                    Ok ( value, testCase ) ->
                        Just
                            ( value
                            , nextSeed seed testCase.seed
                            )

                    Err ( _, testCase ) ->
                        go
                            (tries - 1)
                            (nextSeed seed testCase.seed)
    in
    go 100 initSeed


{-| All fuzzers need to somehow go through picking an `Int`.
([Sequences of Ints](#Minithesis.RandomRun) are what the shrinkers work on,
they're the underlying abstraction.)

This function makes a choice in `[0, n]`, by using the generator if randomness
is needed (or the prefix if some choices are predetermined).

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


{-| Returns a number in the range `[0, n]` (inclusive).
-}
nonnegativeInt_ : Int -> Fuzzer Int
nonnegativeInt_ n =
    Fuzzer (makeChoice n (nonnegativeIntGenerator n))


nonnegativeIntGenerator : Int -> Random.Generator Int
nonnegativeIntGenerator n =
    Random.int 0 n


{-| Returns a `Bool`, with `True` having chance `p` (`[0..1]`).

Input probabilities outside the `[0..1]` range will be clamped to `[0..1]`.

     Fuzz.example (Fuzz.weightedBool 0.75)
     -->
     [True,True,False,True,True,False,True,True,True,True]

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
    -- TODO should we expose this?
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
                    , { testCase | randomRun = RandomRun.append n testCase.randomRun }
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


{-| Returns a `Bool`, with `True` and `False` both picked with the same
probability.

     Fuzz.example Fuzz.bool
     -->
     [False,True,False,False,True,False,False,True,True,False]

-}
bool : Fuzzer Bool
bool =
    oneOfValues [ True, False ]


{-| Returns an `Int` in the range `[from, to]` (inclusive).

The range of supported values is
`[Random.minInt = -2147483648, Random.maxInt = 2147483647]`.

     Fuzz.example (Fuzz.int -4 5)
     -->
     [5,4,1,5,-4,5,3,2,-2,-1]

Rejects `from > to`.

-}
int : Int -> Int -> Fuzzer Int
int from to =
    -- TODO make it shrink towards zero by using `oneOf [map negate ..., ...]`?
    if from > to then
        reject

    else
        nonnegativeInt_ (to - from)
            |> map (\n -> n + from)


{-| Ranges over all `Int`s: `[-2147483648, 2147483647]`

     Fuzz.example Fuzz.anyNumericInt
     -->
     [40734761,724803180,183002667,-764688759,571685512,-765389879,-439862441,-1262912622,1613638464,-799330495]

-}
anyNumericInt : Fuzzer Int
anyNumericInt =
    int Random.minInt Random.maxInt


{-| Ranges over all positive `Int`s: `[1, 2147483647]`

     Fuzz.example Fuzz.positiveInt
     -->
     [40734763,724803182,183002669,1382794890,571685514,1382093770,1707621208,884571027,1613638466,1348153154]

-}
positiveInt : Fuzzer Int
positiveInt =
    int 1 Random.maxInt


{-| Ranges over all negative `Int`s: `[-2147483648, -1]`

     Fuzz.example Fuzz.negativeInt
     -->
     [-2106748887,-1422680468,-1964480981,-764688759,-1575798136,-765389879,-439862441,-1262912622,-533845184,-799330495]

-}
negativeInt : Fuzzer Int
negativeInt =
    int Random.minInt -1


{-| Ranges over all non-positive `Int`s, notably includes zero:
`[-2147483648, 0]`

     Fuzz.example Fuzz.nonpositiveInt
     -->
     [-1806606576,-1337922496,-1936610127,-1099841176,-1254731373,-2106748888,-1422680469,-1964480982,-1575798137,-533845185]

-}
nonpositiveInt : Fuzzer Int
nonpositiveInt =
    int Random.minInt 0


{-| Ranges over all non-negative `Int`s, notably includes zero:
`[0, 2147483647]`

     Fuzz.example Fuzz.nonnegativeInt
     -->
     [40734761,724803180,183002667,1382794889,571685512,1382093769,1707621207,884571026,1613638464,1348153153]

-}
nonnegativeInt : Fuzzer Int
nonnegativeInt =
    int 0 Random.maxInt


intInfinity : Int
intInfinity =
    round (1 / 0)


{-| Ranges over all `Int`s: `[-2147483648, 2147483647]`
and also the `Int` variants of `Infinity`, `-Infinity` and `NaN`.

     Fuzz.example Fuzz.anyInt
     -->
     [-1490358084,-477130762,-1836545270,920695451,210873522,-1448555596,-Infinity,724803180,571685512,-1262912622]

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


convertIntRange :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Float
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
                    avg_

                Nothing ->
                    -- Taken from Python Hypothesis (ListStrategy)
                    -- This deals with the cases where max is Infinity
                    min
                        (max (min__ * 2) (min__ + 5))
                        ((min__ + max__) / 2)

        continueProbability =
            1 - 1 / (1 + average)
    in
    { minLength = min_
    , maxLength = max_
    , continueProbability = continueProbability
    }


{-| Fuzzes a list of random length, with average length of 5.

     Fuzz.example (Fuzz.list (Fuzz.int 0 5))
     -->
     [[0],[2],[],[1,0,1,5],[5,3,0],[3,1,4],[3,0,3,2,2,5],[5,4],[0,1,0,1,2,5,2],[2,0,2,0,2,1,4,1,2,4,0,2,1]]

-}
list : Fuzzer a -> Fuzzer (List a)
list item =
    listWith
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Nothing
        }
        item


{-| Fuzzes a list of given length.

     Fuzz.example (Fuzz.listOfLength 2 (Fuzz.int 0 5))
     -->
     [[5,1],[2,1],[2,3],[1,4],[2,0],[2,1],[5,5],[3,0],[2,3],[1,2]]

-}
listOfLength : Int -> Fuzzer a -> Fuzzer (List a)
listOfLength length item =
    listWith
        { minLength = Just length
        , maxLength = Just length
        , customAverageLength = Just (toFloat length)
        }
        item


{-| Fuzzes a list, giving you options to customize the length distribution.

     Fuzz.exampleWithSeed 10
        (Fuzz.listWith
            { minLength = Just 1
            , maxLength = Just 3
            , customAverageLength = Nothing
            }
            (Fuzz.int 0 5)
        )
     -->
     [[3,0,0],[3,4],[2],[3,0,1],[5],[1,2],[4,2,5],[2,2,4],[3,2],[0,0,0]]

     Fuzz.example
        (Fuzz.listWith
            { minLength = Just 1
            , maxLength = Nothing
            , customAverageLength = Just 2
            }
            Fuzz.char
        )
     -->
     [[3,5,4],[0,3],[2,3],[1],[1],[4,5,5,3,5],[4],[4,3],[1,1,3,5],[1,3,5]]

-}
listWith :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Float
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


{-| Fuzzes a list of random length, guaranteed to contain unique elements, with
average length of 5 items if the item fuzzer allows that.

     Fuzz.example (Fuzz.uniqueList (Fuzz.int 1 3))
     -->
     [[1],[3],[],[3],[],[],[3],[3,2],[3],[1,3,2]]

-}
uniqueList : Fuzzer comparable -> Fuzzer (List comparable)
uniqueList item =
    uniqueListWith
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Nothing
        }
        item


{-| Fuzzes a list of given length, guaranteed to contain unique elements.

     Fuzz.example (Fuzz.uniqueListOfLength 2 (Fuzz.int 1 3))
     -->
     [[3,2],[1,2],[1,2],[3,2],[3,2],[3,1],[3,1],[3,2],[3,1],[2,3]]

Rejects if the item fuzzer cannot satisfy the length requirement.

     Fuzz.example (Fuzz.uniqueListOfLength 3 (Fuzz.int 1 2))
     -->
     [] -- can't generate any examples!

-}
uniqueListOfLength : Int -> Fuzzer comparable -> Fuzzer (List comparable)
uniqueListOfLength length item =
    uniqueListWith
        { minLength = Just length
        , maxLength = Just length
        , customAverageLength = Just (toFloat length)
        }
        item


{-| Fuzzes a list, giving you options to customize the length distribution. The
list items are guaranteed to be unique.

     Fuzz.example
        (Fuzz.uniqueListWith
            { minLength = Just 1
            , maxLength = Just 3
            , customAverageLength = Nothing
            }
            (Fuzz.int 0 5)
        )
     -->
     [[1],[1],[3,5],[0,2,1],[4],[1,2],[2,1,0],[2,4],[5],[1,3,5]]

     Fuzz.example
        (Fuzz.uniqueListWith
            { minLength = Just 1
            , maxLength = Nothing
            , customAverageLength = Just 2
            }
            (Fuzz.int 0 5)
        )
     -->
     [[1,0],[3,5],[5],[3,5,4],[2],[3,4],[0,2,1],[2,1,0,4],[4],[1,3,5]]

Rejects if `min > max`.

Rejects if it can't satisfy the length requirement.

-}
uniqueListWith :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Float
    }
    -> Fuzzer comparable
    -> Fuzzer (List comparable)
uniqueListWith range item =
    uniqueByListWith
        identity
        range
        item


{-| Fuzzes a list of random length, guaranteed to contain unique elements
according to the given key function, with average length of 5 items if the item fuzzer
allows that.

     Fuzz.exampleWithSeed 10
         (Fuzz.uniqueByList (modBy 2) (Fuzz.int 1 5))
     -->
     [[4],[1],[2],[2,1],[3],[],[1],[5],[3],[5]]

-}
uniqueByList : (a -> comparable) -> Fuzzer a -> Fuzzer (List a)
uniqueByList toComparable item =
    uniqueByListWith
        toComparable
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Nothing
        }
        item


{-| Fuzzes a list of given length, guaranteed to contain unique elements
according to the given key function.

     Fuzz.example (Fuzz.uniqueByListOfLength 2 (modBy 2) (Fuzz.int 1 3))
     -->
     [[2,3],[2,3],[3,2],[3,2],[1,2],[1,2],[3,2],[3,2],[3,2],[2,3]]

Rejects if the item fuzzer cannot satisfy the length requirement combined with
the key function.

     Fuzz.example (Fuzz.uniqueListOfLength 3 (modBy 2) (Fuzz.int 1 3))
     -->
     [] -- can't generate any examples!

-}
uniqueByListOfLength : Int -> (a -> comparable) -> Fuzzer a -> Fuzzer (List a)
uniqueByListOfLength length toComparable item =
    uniqueByListWith
        toComparable
        { minLength = Just length
        , maxLength = Just length
        , customAverageLength = Just (toFloat length)
        }
        item


{-| Fuzzes a list, giving you options to customize the length distribution. The
list items are guaranteed to be unique according to the given key function.

     Fuzz.example
        (Fuzz.uniqueByListWith
            (modBy 3)
            { minLength = Just 1
            , maxLength = Just 3
            , customAverageLength = Nothing
            }
            (Fuzz.int 0 5)
        )
     -->
     [[1],[1],[3,5],[0,2,1],[4],[1,2],[2,1,0],[2,4],[5],[1,3,5]]

     Fuzz.example
        (Fuzz.uniqueByListWith
            (modBy 2)
            { minLength = Just 1
            , maxLength = Just 3
            , customAverageLength = Nothing
            }
            (Fuzz.int 0 5)
        )
     -->
     [[4],[0],[0,5],[2],[3,0],[0,3],[2],[4],[4],[1,2]]

Rejects if `min > max`.

Rejects if it can't satisfy the length requirement combined with the key
function.

-}
uniqueByListWith :
    (a -> comparable)
    ->
        { minLength : Maybe Int
        , maxLength : Maybe Int
        , customAverageLength : Maybe Float
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


{-| Combines the two fuzzers into a tuple.

     Fuzz.example (Fuzz.tuple Fuzz.char (Fuzz.int 0 10))
     -->
     [('g',7),('>',7),('2',4),('S',9),('G',7),('2',3),('`',3),('L',4),('{',6),('n',0)]

-}
tuple : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
tuple a b =
    map2 Tuple.pair a b


{-| Combines the three fuzzers into a 3-tuple.

     Fuzz.example (Fuzz.tuple Fuzz.char (Fuzz.int 0 10) Fuzz.bool)
     -->
     [('(',7,True),('-',4,False),('}',3,True),('I',7,False),('D',4,False),('2',9,False),('H',0,False),('2',3,False),('9',1,False),('{',0,False)]

-}
tuple3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
tuple3 a b c =
    map3 (\ax bx cx -> ( ax, bx, cx )) a b c


{-| A fuzzer that always generates the given value.

     Fuzz.example (Fuzz.constant 42)
     -->
     [42,42,42,42,42,42,42,42,42,42]

If you think of the [`Fuzzer`](#Fuzzer) type as `Random.Seed -> Maybe a`, the
`constant` function is how you create `Just` values. (See also
[`reject`](#reject).)

So you can use `constant` in similar patterns as you'd do with
`Json.Decode.succeed` etc.

-}
constant : a -> Fuzzer a
constant a =
    Fuzzer (\testCase -> Ok ( a, testCase ))


{-| A fuzzer that always fails generating a value.

     Fuzz.example Fuzz.reject
     -->
     [] -- fails generating a value

If you think of the [`Fuzzer`](#Fuzzer) type as `Random.Seed -> Maybe a`, the
`reject` function is how you create `Nothing` values. (See also
[`constant`](#constant).)

So you can use `reject` in similar patterns as you'd do with `Json.Decode.fail`
etc.

-}
reject : Fuzzer a
reject =
    Fuzzer (TestCase.markStatus Invalid)


{-| Convert a fuzzed value with a given function.

     Fuzz.example (Fuzz.map (\x -> x * 1000) (Fuzz.int 1 5))
     -->
     [5000,4000,1000,5000,1000,5000,3000,2000,3000,4000]

-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn (Fuzzer fuzzer) =
    Fuzzer
        (\testCase ->
            fuzzer testCase
                |> Result.map (Tuple.mapFirst fn)
        )


{-| Combine two fuzzed values using a given function.

     Fuzz.example (Fuzz.map2 (\a b -> a * b) (Fuzz.int 1 5) (Fuzz.int 1 5)
     -->
     [10,2,16,6,5,20,5,5,6,12]

-}
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


{-| Combine three fuzzed values using a given function.

Handy for record creation if you don't want to use the
`constant ... |> andMap ...` pattern.

-}
map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 fn a b c =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c


{-| Combine four fuzzed values using a given function.

Handy for record creation if you don't want to use the
`constant ... |> andMap ...` pattern.

-}
map4 : (a -> b -> c -> d -> e) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e
map4 fn a b c d =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d


{-| Combine five fuzzed values using a given function.

Handy for record creation if you don't want to use the
`constant ... |> andMap ...` pattern.

-}
map5 : (a -> b -> c -> d -> e -> f) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f
map5 fn a b c d e =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


{-| Combine six fuzzed values using a given function.

Handy for record creation if you don't want to use the
`constant ... |> andMap ...` pattern.

-}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g
map6 fn a b c d e f =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f


{-| Combine seven fuzzed values using a given function.

Handy for record creation if you don't want to use the
`constant ... |> andMap ...` pattern.

-}
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


{-| Combine eight fuzzed values using a given function.

Handy for record creation if you don't want to use the
`constant ... |> andMap ...` pattern.

If you have a record with more than eight fields, don't worry: you can still
fuzz them!

    Fuzz.constant MyRecord
        |> Fuzz.andMap firstFieldFuzzer
        |> Fuzz.andMap secondFieldFuzzer
        |> Fuzz.andMap thirdFieldFuzzer
        |> Fuzz.andMap fourthFieldFuzzer
        |> Fuzz.andMap fifthFieldFuzzer
        |> Fuzz.andMap sixthFieldFuzzer
        |> Fuzz.andMap seventhFieldFuzzer
        |> Fuzz.andMap eighthFieldFuzzer
        |> Fuzz.andMap ninthFieldFuzzer
        |> ...

-}
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


{-| Generate a value and apply a wrapped function to it.

Handy for working with functions that have many arguments (typically record
constructors). See the example in [`map8`](#map8).

-}
andMap : Fuzzer a -> Fuzzer (a -> b) -> Fuzzer b
andMap =
    map2 (|>)


{-| Use a generated value to decide what fuzzer to use next.

For example, let's say you want to generate a list of given length.
One possible way to do that is first choosing how many elements will there be
(generating a number), `andThen` generating a list with that many items:

    Fuzz.int 1 10
        |> Fuzz.andThen
            (\length ->
                let
                    go : Int -> List a -> Fuzzer (List a)
                    go todo acc =
                        if todo <= 0 then
                            constant (List.reverse acc)

                        else
                            itemFuzzer
                                |> Fuzz.andThen (\item -> go (length - 1) (item :: acc))
                in
                go length []
            )

(By the way, it will probably be better to just use one of the [`list`](#list) helpers in
this module.)

Think of it as generalization of [`map`](#map). Inside [`map`](#map) you don't have the option
to fuzz another value based on what you already have; inside `andThen` you do.

-}
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


{-| Only accept values satisfying the given predicate function.

     Fuzz.example
        (Fuzz.int 0 10
            |> Fuzz.filter (\x -> modBy 2 x == 0)
        )
     -->
     [4,4,10,0,0,4,4,6,0,6]

Note that it's often better to get to your wanted values using [`map`](#map), as
you don't run the risk of rejecting too many values and slowing down your tests:

     Fuzz.example
        (Fuzz.int 0 5
            |> Fuzz.map (\x -> x * 2)
        )
     -->
     [2,4,10,10,0,6,6,4,4,2]

-}
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


{-| A trick for writing recursive fuzzers. Wrap the fuzzer into a
`Fuzz.lazy (\_ -> ...)` lambda like this:

    exprFuzzer : Fuzzer Expr
    exprFuzzer =
        Fuzz.frequency
            [ ( 5, Fuzz.map Int Fuzz.anyNumericInt )
            , ( 1
              , Fuzz.map2 Plus
                    (Fuzz.lazy (\_ -> exprFuzzer))
                    (Fuzz.lazy (\_ -> exprFuzzer))
              )
            , ( 1
              , Fuzz.map2 Div
                    (Fuzz.lazy (\_ -> exprFuzzer))
                    (Fuzz.lazy (\_ -> exprFuzzer))
              )
            ]

(Note: if the Elm compiler is complaining about a value depending on itself, or
a dependency cycle, try moving your recursive fuzzer from `let..in` to top level,
or make it a function instead of a value.)

-}
lazy : (() -> Fuzzer a) -> Fuzzer a
lazy fn =
    Fuzzer
        (\testCase ->
            let
                (Fuzzer fuzzer) =
                    fn ()
            in
            fuzzer testCase
        )


{-| Fuzzer always returning the unit value `()`.

     Fuzz.example Fuzz.unit
     -->
     [ (), (), (), (), (), (), (), (), (), () ]

-}
unit : Fuzzer ()
unit =
    constant ()


{-| A fuzzer for `Char` values. Generates random ASCII chars disregarding the
control characters and the extended character set.

The range used for the char codes is 32 to 126.

     Fuzz.example Fuzz.char
     -->
     ['~','2','u','`','9','L','\'','{','\'','n']

-}
char : Fuzzer Char
char =
    -- TODO prefer whitespace?
    charRange 32 126


{-| A fuzzer for `Char` values. Generates random Unicode characters (even
surrogate pairs, but never surrogates themselves). Mostly garbage.

The range used for the char codes is 0 to 1114111 (0x10FFFF).

     Fuzz.example Fuzz.anyChar
     -->
     ['છ','󐅳','񹬫','򔇞','廤','򌠫','񇸀','𙑚','򼦆','򵟵']

-}
anyChar : Fuzzer Char
anyChar =
    -- TODO prefer ASCII and whitespace to unicode?
    charRange 0 maxChar


maxChar : Int
maxChar =
    0x0010FFFF


{-| Use your own char code range for `Char` generation!

     Fuzz.example (Fuzz.charRange 48 57)
     -->
     ['9','8','5','9','0','9','7','6','2','3'] -- 0-9

     Fuzz.example (Fuzz.charRange 65 90)
     -->
     ['N','I','B','V','G','R','T','O','U','J'] -- A-Z

     Fuzz.example (Fuzz.charRange 97 122)
     -->
     ['n','i','b','v','g','r','t','o','u','j'] -- a-z

Rejects if `from > to`.
Rejects if `from < 0 || to < 0`.
Rejects if `from > 0x10FFFF || to > 0x10FFFF`.

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
    -- For these, `charCode |> Char.fromCode |> Char.toCode == NaN`
    charCode >= 0xD800 && charCode <= 0xDFFF


{-| Generates random printable ASCII strings, with average length of 5.

     Fuzz.example Fuzz.string
     -->
     ["x","I","","6a=U",";W?","uDc",":ei_^~","=Y","-NAT\\QJ","{92H2DI}-(KOc"]

-}
string : Fuzzer String
string =
    list char
        |> map String.fromList


{-| Generates random printable ASCII strings of the given length.

     Fuzz.example (Fuzz.stringOfLength 2)
     -->
     ["g)",">D","2P","SE","GH","2~","`u","L9","{'","n'"]

-}
stringOfLength : Int -> Fuzzer String
stringOfLength length =
    listOfLength length char
        |> map String.fromList


{-| Fuzzes a string, giving you options to customize the length distribution and
the internal char fuzzer.

     Fuzz.example
        (Fuzz.stringWith
            { minLength = Just 1
            , maxLength = Just 3
            , customAverageLength = Nothing
            , charFuzzer = Fuzz.char
            }
        )
     -->
     ["`","n","9p","KOc","(","}-",">)U","GE","u","n'`"]

     Fuzz.example
        (Fuzz.stringWith
            { minLength = Just 1
            , maxLength = Nothing
            , customAverageLength = Just 2
            , charFuzzer = Fuzz.charRange 48 57
            }
        )
     -->
     ["392","25","81","9","3","85555","8","67","9131","379"]

-}
stringWith :
    { minLength : Maybe Int
    , maxLength : Maybe Int
    , customAverageLength : Maybe Float
    , charFuzzer : Fuzzer Char
    }
    -> Fuzzer String
stringWith { minLength, maxLength, customAverageLength, charFuzzer } =
    listWith
        { minLength = minLength
        , maxLength = maxLength
        , customAverageLength = customAverageLength
        }
        charFuzzer
        |> map String.fromList


{-| Picks a fuzzer from the list and runs it. All fuzzers have the same chance
to be picked.

     Fuzz.exampleWithSeed 3
        (Fuzz.oneOf
            [ Fuzz.int 10 19
            , Fuzz.int 50 59 |> Fuzz.map negate
            , Fuzz.int 90 99
            ]
        )
     -->
     [-51,-59,12,92,13,91,16,92,14,-57]

-}
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


{-| Picks a value from the list. All values have the same chance to be picked.

     Fuzz.example (Fuzz.oneOfValues [ 42, 999, 1 ])
     -->
     [999,1,1,1,42,42,42,1,1,999]

-}
oneOfValues : List a -> Fuzzer a
oneOfValues items =
    oneOf (List.map constant items)


{-| A more general version of [`oneOf`](#oneOf).

Picks a fuzzer from the list and runs it. The chance to be picked is given for
each fuzzer as a weight.

     Fuzz.example
        (Fuzz.frequency
            [ (10, Fuzz.int 10 19)
            , (5, Fuzz.int 50 59 |> Fuzz.map negate)
            , (1, Fuzz.int 90 99)
            ]
        )
     -->
     [-58,18,13,16,11,18,-50,-58,10,-56]

The weight doesn't need to be integer, nor does the sum of them have to equal to
1:

     Fuzz.example
        (Fuzz.frequency
            [ (1, Fuzz.int 10 19)
            , (0.5, Fuzz.int 50 59 |> Fuzz.map negate)
            , (0.1, Fuzz.int 90 99)
            ]
        )
     -->
     [-58,18,13,16,11,18,-50,-58,10,-56]

-}
frequency : List ( Float, Fuzzer a ) -> Fuzzer a
frequency options =
    let
        cleanOptions : List ( Float, Fuzzer a )
        cleanOptions =
            options
                |> List.filter (\( weight, _ ) -> weight > 0)
    in
    if List.isEmpty cleanOptions then
        reject

    else
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


{-| A more general version of [`oneOfValues`](#oneOfValues).

Picks a value from the list. The chance to be picked is given for each value as
a weight.

     Fuzz.example
        (Fuzz.frequencyValues
            [ (10, 42)
            , (5, 999)
            , (1, 1)
            ]
        )
     -->
     [42,42,42,42,42,42,999,42,42,999]

The weight doesn't need to be integer, nor does the sum of them have to equal to
1:

     Fuzz.example
        (Fuzz.frequencyValues
            [ (1, 42)
            , (0.5, 999)
            , (0.1, 1)
            ]
        )
     -->
     [42,42,42,42,42,42,999,42,42,999]

-}
frequencyValues : List ( Float, a ) -> Fuzzer a
frequencyValues options =
    frequency (List.map (Tuple.mapSecond constant) options)


{-| Randomly returns `Nothing` or wraps the fuzzed value in `Just`.

     Fuzz.example (Fuzz.maybe Fuzz.char)
     -->
     [Nothing,Just 'E',Nothing,Just 'G',Nothing,Just 'u',Nothing,Just 'L',Nothing,Just '\'']

-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe item =
    -- The order here is important: we shrink to the items earlier in the list
    oneOf
        [ constant Nothing
        , map Just item
        ]


{-| Randomly chooses between returning an error value (`Err`) or a success value
(`Ok`).

     Fuzz.example (Fuzz.result Fuzz.bool Fuzz.char)
     -->
     [Ok ')',Err False,Err False,Ok 'E',Err True,Err False,Ok 'u',Ok '9',Err False,Ok '\'']

-}
result : Fuzzer x -> Fuzzer a -> Fuzzer (Result x a)
result errFuzzer okFuzzer =
    oneOf
        [ map Err errFuzzer
        , map Ok okFuzzer
        ]


{-| Returns `Float` in range `[0,1]` inclusive.

     Fuzz.example Fuzz.probability
     -->
     [0.0857102531109948,0.10466259812416492,0.20333718837701903,0.10971943162142586,0.394916603751272,0.22617770145826405,0.7359710146032609,0.06733096418943953,0.5926689124932717,0.69903743334244]

-}
probability : Fuzzer Float
probability =
    tuple int32 int32
        |> map Float.fractionalFloat


{-| Returns a `Float` in the range `[from, to]` (inclusive).

The range of supported values is
`[-1.7976931348623157e308, 1.7976931348623157e308]`.

     Fuzz.example (Fuzz.float 10 20)
     -->
     [10.857102531109948,11.046625981241649,12.03337188377019,11.097194316214258,13.94916603751272,12.261777014582641,17.359710146032608,10.673309641894395,15.926689124932718,16.9903743334244]

Rejects `from > to`.

-}
float : Float -> Float -> Fuzzer Float
float from to =
    -- TODO can this shrink to the nice integer-y floats?
    floatWith
        { min = Just from
        , max = Just to
        , allowNaN = False
        , allowInfinities = False
        }


{-| Ranges over all possible `Float`s:
`[-1.7976931348623157e308, 1.7976931348623157e308]`
except for `Infinity`, `-Infinity` and `NaN`.

Prefers nice non-fractional floats and shrinks to them.

Also sprinkles some nasty edge-case floats in for good measure.

     Fuzz.example Fuzz.anyNumericFloat
     -->
     [-1.1,9007199254740992,2.2250738585072014e-308,-2.220446049250313e-16,627908,-3.5502481189047345e+60,-1.192092896e-7,0.00001,-10000000,-1.192092896e-7]

-}
anyNumericFloat : Fuzzer Float
anyNumericFloat =
    anyFloatWith
        { allowNaN = False
        , allowInfinities = False
        }


{-| Ranges over all possible `Float`s:
`[-1.7976931348623157e308, 1.7976931348623157e308]`
and also the `Infinity`, `-Infinity` and `NaN`.

Prefers nice non-fractional floats and shrinks to them.

Also sprinkles some nasty edge-case floats in for good measure.

     Fuzz.example Fuzz.anyFloat
     -->
     [Infinity,1.1,-1.192092896e-7,-Infinity,627908,-3.5502481189047345e+60,Infinity,-Infinity,NaN,NaN]

-}
anyFloat : Fuzzer Float
anyFloat =
    anyFloatWith
        { allowNaN = True
        , allowInfinities = True
        }


nanOrInfiniteFloat :
    { nan : Bool
    , positiveInfinity : Bool
    , negativeInfinity : Bool
    }
    -> Fuzzer Float
nanOrInfiniteFloat { nan, positiveInfinity, negativeInfinity } =
    [ if nan then
        Just (0 / 0)

      else
        Nothing
    , if positiveInfinity then
        Just (1 / 0)

      else
        Nothing
    , if negativeInfinity then
        Just (-1 / 0)

      else
        Nothing
    ]
        |> List.filterMap identity
        |> oneOfValues


anyFloatWith : { allowNaN : Bool, allowInfinities : Bool } -> Fuzzer Float
anyFloatWith { allowNaN, allowInfinities } =
    {- TODO should we expose this?
       Make sense of the floats hieararchy and what shrinks how.
    -}
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
    oneOf
        [ wellShrinkingFloat { allowInfinities = allowInfinities }
        , oneOfValues nastyFloats
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


scaledFloat : Float -> Float -> Fuzzer Float
scaledFloat min max =
    if min > max then
        reject

    else
        probability
            |> map (\f -> f * (max - min) + min)
            {- TODO Infinite values can happen for the above calculation
               eg. in case of
                   min = -1.7976931348623157e+308
                   max =  1.7976931348623157e+308
               Do we have a better way to generate these floats?
            -}
            |> filter (not << isInfinite)


{-| Fuzzes a `Float`, giving you the options to customize the range and presence
of certain edge-case values.

     Fuzz.example
        (Fuzz.floatWith
            { min = Nothing
            , max = Just 0
            , allowNaN = False
            , allowInfinities = True
            }
        )
     -->
     [-Infinity,-3.3348079349305726e+293,-865864,-377335,-1.7976931348623157e+308,-3.5502481189047345e+60,-Infinity,-Infinity,0,-70601]

-}
floatWith :
    { min : Maybe Float
    , max : Maybe Float
    , allowNaN : Bool
    , allowInfinities : Bool
    }
    -> Fuzzer Float
floatWith { min, max, allowNaN, allowInfinities } =
    -- TODO add nasty floats to config of this function?
    {- TODO if we figure out how to do nextUp and nextDown for IEEE 734 floats,
       we'll be able to do excludeMin : Bool, excludeMax : Bool
    -}
    let
        canGenerateNanOrInfiniteFloat : Bool
        canGenerateNanOrInfiniteFloat =
            allowNaN
                || (allowInfinities
                        && ((min == Just (-1 / 0))
                                || (min == Nothing)
                                || (max == Just (1 / 0))
                                || (max == Nothing)
                           )
                   )
    in
    case ( min, max ) of
        ( Nothing, Nothing ) ->
            anyFloatWith
                { allowNaN = allowNaN
                , allowInfinities = allowInfinities
                }

        ( Just min_, Nothing ) ->
            if min_ < 0 then
                [ Just <| map abs anyNumericFloat
                , Just <| scaledFloat min_ -0.0
                , if canGenerateNanOrInfiniteFloat then
                    Just <|
                        nanOrInfiniteFloat
                            { positiveInfinity = True
                            , negativeInfinity = min_ == (-1 / 0)
                            , nan = allowNaN
                            }

                  else
                    Nothing
                ]
                    |> List.filterMap identity
                    |> oneOf

            else
                map (\f -> min_ + abs f) anyNumericFloat

        ( Nothing, Just max_ ) ->
            if max_ >= 0 then
                [ Just <| scaledFloat 0.0 max_
                , Just <| map (abs >> negate) anyNumericFloat
                , if canGenerateNanOrInfiniteFloat then
                    Just <|
                        nanOrInfiniteFloat
                            { positiveInfinity = max_ == (1 / 0)
                            , negativeInfinity = True
                            , nan = allowNaN
                            }

                  else
                    Nothing
                ]
                    |> List.filterMap identity
                    |> oneOf

            else
                map (\f -> max_ - abs f) anyNumericFloat

        ( Just min_, Just max_ ) ->
            if min_ > max_ then
                reject

            else if isNaN min_ || isNaN max_ || isInfinite min_ || isInfinite max_ then
                reject

            else if min_ == max_ then
                constant min_

            else if canGenerateNanOrInfiniteFloat then
                oneOf
                    [ scaledFloat min_ max_
                    , nanOrInfiniteFloat
                        { positiveInfinity = max_ == (1 / 0)
                        , negativeInfinity = min_ == (-1 / 0)
                        , nan = allowNaN
                        }
                    ]

            else
                scaledFloat min_ max_
