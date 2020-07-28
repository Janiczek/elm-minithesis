module MinithesisTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Minithesis exposing (TestResult(..))
import Minithesis.Fuzz as F exposing (Fuzzer)
import Minithesis.Stop exposing (Stop(..))
import Random
import Set
import Shrink
import Test exposing (Test, describe, fuzz, only, test, todo)


suite : Test
suite =
    describe "Minithesis"
        [ general
        , fuzzers
        , shrinkers
        ]


seedFuzzer : Fuzz.Fuzzer Int
seedFuzzer =
    Fuzz.custom
        (Random.int 0 Random.maxInt)
        Shrink.noShrink


testMinithesis : String -> Fuzzer a -> (a -> Bool) -> TestResult a -> Test
testMinithesis name fuzzer userFn expectedResult =
    fuzz seedFuzzer name <|
        \seed ->
            Minithesis.test "" fuzzer userFn
                |> Minithesis.run seed
                |> Tuple.second
                |> Expect.equal expectedResult


testMinithesisRejects : String -> Fuzzer a -> Test
testMinithesisRejects name fuzzer =
    testMinithesis ("Rejects: " ++ name)
        fuzzer
        (\_ -> True)
        (Error Unsatisfiable)


testMinithesisCanGenerate : String -> Fuzzer a -> a -> Test
testMinithesisCanGenerate name fuzzer value =
    testMinithesis ("Can generate: " ++ name)
        fuzzer
        (\v -> v /= value)
        (FailsWith value)


testMinithesisCanGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
testMinithesisCanGenerateSatisfying name fuzzer predicate =
    fuzz seedFuzzer name <|
        \seed ->
            Minithesis.test
                ("Can generate value satisfying: " ++ name)
                fuzzer
                (not << predicate)
                |> Minithesis.run seed
                |> Tuple.second
                |> Minithesis.isFailsWith
                |> Expect.true "Should have given an example of a generated value satisfying the predicate"


testMinithesisCannotGenerate : String -> Fuzzer a -> a -> Test
testMinithesisCannotGenerate name fuzzer value =
    fuzz seedFuzzer name <|
        \seed ->
            Minithesis.test
                ("Cannot generate: " ++ name)
                fuzzer
                (\v -> v == value)
                |> Minithesis.run seed
                |> Tuple.second
                |> Expect.notEqual Passes


testMinithesisCannotGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
testMinithesisCannotGenerateSatisfying name fuzzer predicate =
    fuzz seedFuzzer name <|
        \seed ->
            Minithesis.test
                ("Cannot generate: " ++ name)
                fuzzer
                predicate
                |> Minithesis.run seed
                |> Tuple.second
                |> Expect.notEqual Passes


general : Test
general =
    describe "General"
        [ test "Running a test returns the label as first item in the tuple" <|
            \() ->
                Minithesis.test "label" F.reject (\_ -> True)
                    |> Minithesis.run 0
                    |> Tuple.first
                    |> Expect.equal "label"
        ]


fuzzers : Test
fuzzers =
    describe "Minithesis.Fuzz"
        [ describe "bool"
            [ testMinithesisCanGenerate "False" F.bool False
            , testMinithesisCanGenerate "True" F.bool True
            ]
        , describe "weightedBool"
            [ testMinithesisCanGenerate "0.5 False" (F.weightedBool 0.5) False
            , testMinithesisCanGenerate "0.5 True" (F.weightedBool 0.5) True
            , testMinithesis "0 = always False"
                (F.weightedBool 0)
                (\bool -> not bool)
                Passes
            , testMinithesis "1 = always True"
                (F.weightedBool 1)
                (\bool -> bool)
                Passes
            , testMinithesis "<0 clamps to 0"
                (F.weightedBool -0.5)
                (\bool -> not bool)
                Passes
            , testMinithesis ">1 clamps to 1"
                (F.weightedBool 1.5)
                (\bool -> bool)
                Passes
            ]
        , describe "unit"
            [ testMinithesisCanGenerate "()" F.unit ()
            ]
        , describe "constant"
            [ testMinithesis "Returns what you give it - Unit"
                (F.constant ())
                (\value -> value == ())
                Passes
            , testMinithesis "Returns what you give it - any other type (here Int)"
                (F.constant 5)
                (\value -> value == 5)
                Passes
            ]
        , describe "maybe"
            [ testMinithesisCanGenerateSatisfying "Just" (F.maybe F.unit) ((/=) Nothing)
            , testMinithesisCanGenerateSatisfying "Nothing" (F.maybe F.unit) ((==) Nothing)
            ]
        , describe "result"
            [ testMinithesisCanGenerateSatisfying "Ok" (F.result F.unit F.unit) (Result.toMaybe >> (/=) Nothing)
            , testMinithesisCanGenerateSatisfying "Err" (F.result F.unit F.unit) (Result.toMaybe >> (==) Nothing)
            ]
        , describe "filter"
            [ testMinithesis "Stops with Unsatisfiable if rejecting too many values"
                (F.int 0 10 |> F.filter (\_ -> False))
                (\_ -> True)
                (Error Unsatisfiable)
            , testMinithesis "Generated values satisfy preconditions"
                (F.int 0 10 |> F.filter (\n -> n /= 0))
                (\n -> n /= 0)
                Passes
            , testMinithesis "Another example"
                (F.int 0 10 |> F.filter (\n -> modBy 2 n == 0))
                (\n -> modBy 2 n == 0)
                Passes
            ]
        , describe "reject"
            [ testMinithesis "Makes the test never run"
                F.reject
                (\n -> True)
                (Error Unsatisfiable)
            ]
        , describe "map"
            [ testMinithesis "any number * 2 = even number"
                (F.int 0 5 |> F.map (\n -> n * 2))
                (\n -> modBy 2 n == 0)
                Passes
            ]
        , describe "int"
            [ testMinithesis "Can work with negative numbers"
                (F.int -10 10)
                (\n -> n >= -10 && n <= 10)
                Passes
            , testMinithesisRejects "min > max"
                (F.int (Random.minInt + 1) Random.maxInt
                    |> F.andThen
                        (\from ->
                            F.int Random.minInt (from - 1)
                                |> F.andThen (F.int from)
                        )
                )
            , testMinithesis "Random.minInt"
                (F.int -2147483648 -2147483648)
                (\n -> n == -2147483648)
                Passes
            , testMinithesis "Random.maxInt"
                (F.int 2147483647 2147483647)
                (\n -> n == 2147483647)
                Passes
            , testMinithesis "Full range"
                (F.int -2147483648 2147483647)
                (\n -> n >= -2147483648 && n <= 2147483647)
                Passes
            ]
        , describe "anyInt"
            [ testMinithesis "Full range"
                F.anyInt
                (\n ->
                    isInfinite (toFloat n)
                        || isNaN (toFloat n)
                        || (n >= -2147483648 && n <= 2147483647)
                )
                Passes
            , testMinithesisCanGenerateSatisfying "any Infinity" F.anyInt (toFloat >> isInfinite)
            , testMinithesisCanGenerateSatisfying "NaN" F.anyInt (toFloat >> isNaN)
            ]
        , describe "anyNumericInt"
            [ testMinithesis "Full range"
                F.anyNumericInt
                (\n -> n >= -2147483648 && n <= 2147483647)
                Passes
            , testMinithesis "Cannot generate any Infinity"
                F.anyNumericInt
                (not << isInfinite << toFloat)
                Passes
            , testMinithesis "Cannot generate NaN"
                F.anyNumericInt
                (not << isNaN << toFloat)
                Passes
            ]
        , describe "positiveInt"
            [ testMinithesisCannotGenerateSatisfying "negative int"
                F.positiveInt
                (\n -> n < 0)
            , testMinithesisCannotGenerate "zero" F.positiveInt 0
            ]
        , describe "negativeInt"
            [ testMinithesisCannotGenerateSatisfying "positive int"
                F.negativeInt
                (\n -> n > 0)
            , testMinithesisCannotGenerate "zero" F.negativeInt 0
            ]
        , describe "nonpositiveInt"
            [ testMinithesisCannotGenerateSatisfying "positive int"
                F.nonpositiveInt
                (\n -> n > 0)

            -- TODO kinda hard to do such a test - depends on too much luck:
            --, testMinithesisCanGenerate "zero" F.nonpositiveInt 0
            ]
        , describe "nonnegativeInt"
            [ testMinithesisCannotGenerateSatisfying "negative int"
                F.nonnegativeInt
                (\n -> n < 0)

            -- TODO kinda hard to do such a test - depends on too much luck:
            --, testMinithesisCanGenerate "zero" F.nonnegativeInt 0
            ]
        , describe "probability"
            [ testMinithesis "Range 0..1"
                F.probability
                (\n -> n >= 0 && n <= 1)
                Passes
            , testMinithesisCannotGenerateSatisfying "NaN" F.probability isNaN
            , testMinithesisCannotGenerateSatisfying "Infinities" F.probability isInfinite
            ]
        , describe "float"
            (let
                arbitraryFloat : Fuzzer ( ( Float, Float ), Float )
                arbitraryFloat =
                    F.int Random.minInt Random.maxInt
                        |> F.andThen
                            (\from ->
                                F.int from Random.maxInt
                                    |> F.andThen
                                        (\to ->
                                            let
                                                from_ =
                                                    toFloat from

                                                to_ =
                                                    toFloat to
                                            in
                                            F.tuple
                                                (F.constant ( from_, to_ ))
                                                (F.float from_ to_)
                                        )
                            )
             in
             [ testMinithesis "Given range"
                arbitraryFloat
                (\( ( from, to ), float ) ->
                    float >= from && float <= to
                )
                Passes
             , testMinithesisCannotGenerateSatisfying "NaN"
                arbitraryFloat
                (\( _, float ) -> isNaN float)
             , testMinithesisCannotGenerateSatisfying "infinities"
                arbitraryFloat
                (\( _, float ) -> isInfinite float)
             , testMinithesisRejects "min > max"
                (F.int (Random.minInt + 1) Random.maxInt
                    |> F.andThen
                        (\from ->
                            F.int Random.minInt (from - 1)
                                |> F.andThen (\to -> F.float (toFloat from) (toFloat to))
                        )
                )
             , testMinithesisRejects "from = NaN"
                (F.int Random.minInt Random.maxInt
                    |> F.andThen (\to -> F.float (0 / 0) (toFloat to))
                )
             , testMinithesisRejects "from = +Inf"
                (F.int Random.minInt Random.maxInt
                    |> F.andThen (\to -> F.float (1 / 0) (toFloat to))
                )
             , testMinithesisRejects "from = -Inf"
                (F.int Random.minInt Random.maxInt
                    |> F.andThen (\to -> F.float (-1 / 0) (toFloat to))
                )
             , testMinithesisRejects "to = NaN"
                (F.int Random.minInt Random.maxInt
                    |> F.andThen (\from -> F.float (toFloat from) (0 / 0))
                )
             , testMinithesisRejects "to = +Inf"
                (F.int Random.minInt Random.maxInt
                    |> F.andThen (\from -> F.float (toFloat from) (1 / 0))
                )
             , testMinithesisRejects "to = -Inf"
                (F.int Random.minInt Random.maxInt
                    |> F.andThen (\from -> F.float (toFloat from) (-1 / 0))
                )
             ]
            )
        , describe "anyNumericFloat"
            [ testMinithesisCannotGenerateSatisfying "NaN" F.anyNumericFloat isNaN
            , testMinithesisCannotGenerateSatisfying "infinities" F.anyNumericFloat isInfinite
            , testMinithesis "Full range"
                F.anyNumericFloat
                (\f -> f >= -1.7976931348623157e308 && f <= 1.7976931348623157e308)
                Passes
            ]
        , describe "anyFloat"
            [ testMinithesisCanGenerateSatisfying "NaN" F.anyFloat isNaN
            , testMinithesisCanGenerateSatisfying "infinities" F.anyFloat isInfinite
            , testMinithesis "Full range"
                (F.anyFloat |> F.filter (\f -> not (isInfinite f || isNaN f)))
                (\f -> f >= -1.7976931348623157e308 && f <= 1.7976931348623157e308)
                Passes
            ]
        , describe "floatWith"
            [ testMinithesisCanGenerateSatisfying "NaN if allowNaN = True"
                (F.floatWith
                    { min = Nothing
                    , max = Nothing
                    , allowNaN = True
                    , allowInfinities = False
                    }
                )
                isNaN
            , testMinithesisCannotGenerateSatisfying "NaN if allowNaN = False"
                (F.floatWith
                    { min = Nothing
                    , max = Nothing
                    , allowNaN = False
                    , allowInfinities = False
                    }
                )
                isNaN
            , testMinithesisCanGenerateSatisfying "infinities if allowInfinities = True"
                (F.floatWith
                    { min = Nothing
                    , max = Nothing
                    , allowNaN = False
                    , allowInfinities = True
                    }
                )
                isInfinite
            , testMinithesisCannotGenerateSatisfying "infinities if allowInfinities = False"
                (F.floatWith
                    { min = Nothing
                    , max = Nothing
                    , allowNaN = False
                    , allowInfinities = False
                    }
                )
                isInfinite
            , testMinithesis "Full range"
                (F.floatWith
                    { min = Nothing
                    , max = Nothing
                    , allowNaN = False
                    , allowInfinities = False
                    }
                )
                (\f -> f >= -1.7976931348623157e308 && f <= 1.7976931348623157e308)
                Passes
            , testMinithesis "min works"
                (F.anyNumericFloat
                    |> F.andThen
                        (\min ->
                            F.tuple
                                (F.constant min)
                                (F.floatWith
                                    { min = Just min
                                    , max = Nothing
                                    , allowNaN = False
                                    , allowInfinities = False
                                    }
                                )
                        )
                )
                (\( min, f ) -> f >= min)
                Passes
            , testMinithesis "max works"
                (F.anyNumericFloat
                    |> F.andThen
                        (\max ->
                            F.tuple
                                (F.constant max)
                                (F.floatWith
                                    { min = Nothing
                                    , max = Just max
                                    , allowNaN = False
                                    , allowInfinities = False
                                    }
                                )
                        )
                )
                (\( max, f ) -> f <= max)
                Passes
            , testMinithesisCanGenerateSatisfying "NaN with min/max = Just"
                (F.tuple
                    (F.maybe F.anyNumericFloat)
                    (F.maybe F.anyNumericFloat)
                    |> F.andThen
                        (\( min, max ) ->
                            F.floatWith
                                { min = min
                                , max = max
                                , allowNaN = True
                                , allowInfinities = False
                                }
                        )
                )
                isNaN
            , testMinithesisCanGenerateSatisfying "infinities with min/max = Just"
                (F.tuple
                    (F.maybe F.anyNumericFloat)
                    (F.maybe F.anyNumericFloat)
                    |> F.andThen
                        (\( min, max ) ->
                            F.floatWith
                                { min = min
                                , max = max
                                , allowNaN = False
                                , allowInfinities = True
                                }
                        )
                )
                isInfinite
            ]
        , describe "char"
            [ testMinithesis "Range 32..126"
                F.char
                (\char ->
                    let
                        code =
                            Char.toCode char
                    in
                    code >= 32 && code <= 126
                )
                Passes
            ]
        , describe "anyChar"
            [ testMinithesis "Range 0..0x10FFFF"
                F.anyChar
                (\char ->
                    let
                        code =
                            Char.toCode char
                    in
                    code >= 0 && code <= 0x0010FFFF
                )
                Passes
            ]
        , describe "charRange"
            [ testMinithesis "Given range"
                (F.int 0 0x0010FFFF
                    |> F.andThen
                        (\from ->
                            F.int from 0x0010FFFF
                                |> F.andThen
                                    (\to ->
                                        F.tuple
                                            (F.constant ( from, to ))
                                            (F.charRange from to)
                                    )
                        )
                )
                (\( ( from, to ), char ) ->
                    let
                        code =
                            Char.toCode char
                    in
                    code >= from && code <= to
                )
                Passes
            , testMinithesisRejects "Negative range"
                (F.negativeInt
                    |> F.andThen
                        (\negFrom ->
                            F.int negFrom -1
                                |> F.andThen (F.charRange negFrom)
                        )
                )
            , testMinithesisRejects "from > to"
                (F.nonnegativeInt
                    |> F.andThen
                        (\to ->
                            F.int (to + 1) Random.maxInt
                                |> F.andThen (\from -> F.charRange from to)
                        )
                )
            , testMinithesisCannotGenerateSatisfying "surrogates (0xD800..0xDFFF)"
                (F.int 0 0x0010FFFF
                    |> F.andThen
                        (\from ->
                            F.int from 0x0010FFFF
                                |> F.andThen (F.charRange from)
                        )
                )
                (\c ->
                    let
                        code =
                            Char.toCode c
                    in
                    code >= 0xD800 && code <= 0xDFFF
                )
            , testMinithesisCannotGenerate "replacement char (0xFFFD)"
                (F.int 0 0x0010FFFF
                    |> F.andThen
                        (\from ->
                            F.int from 0x0010FFFF
                                |> F.andThen (F.charRange from)
                        )
                )
                '�'
            ]
        , describe "string"
            [ testMinithesisCanGenerate "empty string" F.string ""
            , testMinithesisCanGenerateSatisfying "nonempty string" F.string (not << String.isEmpty)
            ]
        , describe "stringOfLength"
            [ testMinithesis "negative length -> empty list"
                (F.negativeInt
                    |> F.andThen F.stringOfLength
                )
                (\string -> String.isEmpty string)
                Passes
            , testMinithesis "always the specified length"
                (F.int 0 10
                    |> F.andThen
                        (\length ->
                            F.tuple
                                (F.constant length)
                                (F.stringOfLength length)
                        )
                )
                (\( length, string ) -> String.length string == length)
                Passes
            ]
        , describe "stringWith"
            [ testMinithesisCanGenerateSatisfying "string containing a char from different range if given a different fuzzer"
                (F.stringWith
                    { minLength = Just 1
                    , maxLength = Just 10
                    , charFuzzer = F.anyChar
                    }
                )
                (\string ->
                    string
                        |> String.toList
                        |> List.any
                            (\c ->
                                let
                                    code =
                                        Char.toCode c
                                in
                                code < 32 || code > 126
                            )
                )
            , testMinithesis "empty string"
                (F.stringWith
                    { minLength = Nothing
                    , maxLength = Just 0
                    , charFuzzer = F.char
                    }
                )
                (\string -> String.isEmpty string)
                Passes
            , testMinithesis "single char string"
                (F.stringWith
                    { minLength = Just 1
                    , maxLength = Just 1
                    , charFuzzer = F.char
                    }
                )
                (\string -> String.length string == 1)
                Passes
            , testMinithesisCanGenerateSatisfying "one to three chars: 1"
                (F.stringWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , charFuzzer = F.char
                    }
                )
                (\string -> String.length string == 1)
            , testMinithesisCanGenerateSatisfying "one to three chars: 2"
                (F.stringWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , charFuzzer = F.char
                    }
                )
                (\string -> String.length string == 2)
            , testMinithesisCanGenerateSatisfying "one to three chars: 3"
                (F.stringWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , charFuzzer = F.char
                    }
                )
                (\string -> String.length string == 3)
            , testMinithesisRejects "min > max"
                (F.int 0 10
                    |> F.andThen
                        (\to ->
                            F.int (to + 1) (to + 10)
                                |> F.andThen
                                    (\from ->
                                        F.stringWith
                                            { minLength = Just from
                                            , maxLength = Just to
                                            , charFuzzer = F.char
                                            }
                                    )
                        )
                )
            ]
        , describe "oneOfValues"
            [ testMinithesisCanGenerate "element of the list: 1" (F.oneOfValues [ 1, 42 ]) 1
            , testMinithesisCanGenerate "element of the list: 42" (F.oneOfValues [ 1, 42 ]) 42
            , testMinithesis "One value -> picks it"
                (F.oneOfValues [ 42 ])
                (\n -> n == 42)
                Passes
            , testMinithesisRejects "Empty list"
                (F.oneOfValues [])
            ]
        , describe "oneOf"
            (let
                fuzzer : Fuzzer Int
                fuzzer =
                    F.oneOf
                        [ F.int -2 0
                        , F.constant 2
                        ]
             in
             [ testMinithesisCanGenerate "possible int: -2" fuzzer -2
             , testMinithesisCanGenerate "possible int: -1" fuzzer -1
             , testMinithesisCanGenerate "possible int: 0" fuzzer 0
             , testMinithesisCanGenerate "possible int: 2" fuzzer 2
             , testMinithesis "One fuzzer -> picks it"
                (F.oneOf [ F.unit ])
                (\n -> n == ())
                Passes
             , testMinithesisRejects "Empty list"
                (F.oneOf [])
             ]
            )
        , describe "frequencyValues"
            (let
                fuzzer : Fuzzer Int
                fuzzer =
                    F.frequencyValues
                        [ ( 0.3, 1 )
                        , ( 0.7, 42 )
                        ]
             in
             [ testMinithesisCanGenerate "element of the list: 1" fuzzer 1
             , testMinithesisCanGenerate "element of the list: 42" fuzzer 42
             , testMinithesis "One value -> picks it"
                (F.frequencyValues [ ( 0.7, 42 ) ])
                (\n -> n == 42)
                Passes
             , testMinithesisRejects "Empty list"
                (F.frequencyValues [])
             ]
            )
        , describe "frequency"
            (let
                fuzzer : Fuzzer Int
                fuzzer =
                    F.frequency
                        [ ( 0.3, F.int -2 0 )
                        , ( 0.7, F.constant 2 )
                        ]
             in
             [ testMinithesisCanGenerate "possible int: -2" fuzzer -2
             , testMinithesisCanGenerate "possible int: -1" fuzzer -1
             , testMinithesisCanGenerate "possible int: 0" fuzzer 0
             , testMinithesisCanGenerate "possible int: 2" fuzzer 2
             , testMinithesis "One fuzzer -> picks it"
                (F.frequency [ ( 0.3, F.unit ) ])
                (\n -> n == ())
                Passes
             , testMinithesisRejects "Empty list"
                (F.frequency [])
             ]
            )
        , describe "andThen"
            [ testMinithesis "Integer defined by another integer"
                (F.int 0 5
                    |> F.andThen
                        (\m ->
                            F.tuple
                                (F.constant m)
                                (F.int m (m + 10))
                        )
                )
                (\( m, n ) -> m <= n && n <= m + 10)
                Passes
            ]
        , describe "list"
            [ testMinithesisCanGenerate "empty list" (F.list F.unit) []
            , testMinithesisCanGenerateSatisfying "nonempty list" (F.list F.unit) (not << List.isEmpty)
            ]
        , describe "listWith"
            [ testMinithesis "empty list"
                (F.listWith
                    { minLength = Nothing
                    , maxLength = Just 0
                    , customAverageLength = Nothing
                    }
                    F.unit
                )
                (\list -> List.isEmpty list)
                Passes
            , testMinithesis "single item list"
                (F.listWith
                    { minLength = Just 1
                    , maxLength = Just 1
                    , customAverageLength = Nothing
                    }
                    F.unit
                )
                (\list -> List.length list == 1)
                Passes
            , testMinithesisCanGenerateSatisfying "one to three items: 1"
                (F.listWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , customAverageLength = Nothing
                    }
                    F.unit
                )
                (\list -> List.length list == 1)
            , testMinithesisCanGenerateSatisfying "one to three items: 2"
                (F.listWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , customAverageLength = Nothing
                    }
                    F.unit
                )
                (\list -> List.length list == 2)
            , testMinithesisCanGenerateSatisfying "one to three items: 3"
                (F.listWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , customAverageLength = Nothing
                    }
                    F.unit
                )
                (\list -> List.length list == 3)
            , testMinithesisRejects "min > max"
                (F.int 0 10
                    |> F.andThen
                        (\to ->
                            F.int (to + 1) (to + 10)
                                |> F.andThen
                                    (\from ->
                                        F.listWith
                                            { minLength = Just from
                                            , maxLength = Just to
                                            , customAverageLength = Nothing
                                            }
                                            F.unit
                                    )
                        )
                )
            ]
        , describe "listOfLength"
            [ testMinithesis "always the specified length"
                (F.int 0 10
                    |> F.andThen
                        (\length ->
                            F.tuple
                                (F.constant length)
                                (F.listOfLength length F.unit)
                        )
                )
                (\( length, list ) -> List.length list == length)
                Passes
            , testMinithesis "negative length -> empty list"
                (F.negativeInt
                    |> F.andThen (\length -> F.listOfLength length F.unit)
                )
                (\list -> list == [])
                Passes
            ]
        , describe "uniqueList"
            [ testMinithesis "elements are unique"
                (F.uniqueList F.anyInt)
                (\list -> Set.size (Set.fromList list) == List.length list)
                Passes
            ]
        , describe "uniqueListOfLength"
            [ testMinithesis "always the specified length"
                (F.int 0 10
                    |> F.andThen
                        (\length ->
                            F.tuple
                                (F.constant length)
                                (F.uniqueListOfLength length F.anyInt)
                        )
                )
                (\( length, list ) -> List.length list == length)
                Passes
            , testMinithesisRejects "Domain not large enough"
                (F.uniqueListOfLength 5 (F.int 1 3))
            , testMinithesis "negative length -> empty list"
                (F.negativeInt
                    |> F.andThen (\length -> F.uniqueListOfLength length (F.int 1 10))
                )
                (\list -> list == [])
                Passes
            ]
        , describe "uniqueListWith"
            [ testMinithesis "empty list"
                (F.uniqueListWith
                    { minLength = Nothing
                    , maxLength = Just 0
                    , customAverageLength = Nothing
                    }
                    (F.int 1 1)
                )
                (\list -> List.isEmpty list)
                Passes
            , testMinithesis "single item list"
                (F.uniqueListWith
                    { minLength = Just 1
                    , maxLength = Just 1
                    , customAverageLength = Nothing
                    }
                    (F.int 1 1)
                )
                (\list -> List.length list == 1)
                Passes
            , testMinithesisCanGenerateSatisfying "one to three items: 1"
                (F.uniqueListWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , customAverageLength = Nothing
                    }
                    (F.int 1 10)
                )
                (\list -> List.length list == 1)
            , testMinithesisCanGenerateSatisfying "one to three items: 2"
                (F.uniqueListWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , customAverageLength = Nothing
                    }
                    (F.int 1 10)
                )
                (\list -> List.length list == 2)
            , testMinithesisCanGenerateSatisfying "one to three items: 3"
                (F.uniqueListWith
                    { minLength = Just 1
                    , maxLength = Just 3
                    , customAverageLength = Nothing
                    }
                    (F.int 1 10)
                )
                (\list -> List.length list == 3)
            , testMinithesis "if given wiggling space does what it can"
                (F.uniqueListWith
                    { minLength = Just 1
                    , maxLength = Nothing
                    , customAverageLength = Nothing
                    }
                    (F.int 1 1)
                )
                (\list -> List.length list == 1)
                Passes
            , testMinithesisRejects "Domain not large enough"
                (F.uniqueListWith
                    { minLength = Just 2
                    , maxLength = Just 2
                    , customAverageLength = Nothing
                    }
                    (F.int 1 1)
                )
            , testMinithesisRejects "min > max"
                (F.int 0 10
                    |> F.andThen
                        (\to ->
                            F.int (to + 1) (to + 10)
                                |> F.andThen
                                    (\from ->
                                        F.uniqueListWith
                                            { minLength = Just from
                                            , maxLength = Just to
                                            , customAverageLength = Nothing
                                            }
                                            (F.int 1 20)
                                    )
                        )
                )
            ]
        ]


shrinkers : Test
shrinkers =
    describe "Shrinkers"
        [ testMinithesis "Able to reduce additive pairs"
            (F.tuple
                (F.int 0 1000)
                (F.int 0 1000)
            )
            (\( m, n ) -> m + n <= 1000)
            (FailsWith ( 1, 1000 ))
        , shrinkingChallenges
        ]


shrinkingChallenges : Test
shrinkingChallenges =
    describe "Shrinking challenges"
        -- https://github.com/jlink/shrinking-challenge
        [ todo "Bound 5" -- https://github.com/jlink/shrinking-challenge/blob/main/challenges/bound5.md
        , todo "Reverse" -- https://github.com/jlink/shrinking-challenge/blob/main/challenges/reverse.md
        , todo "Large union list" -- https://github.com/jlink/shrinking-challenge/blob/main/challenges/large_union_list.md
        , todo "Calculator" -- https://github.com/jlink/shrinking-challenge/blob/main/challenges/calculator.md
        ]
