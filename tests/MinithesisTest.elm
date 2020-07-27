module MinithesisTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Minithesis exposing (TestResult(..))
import Minithesis.Fuzz as F exposing (Fuzzer)
import Minithesis.Stop exposing (Stop(..))
import Set
import Test exposing (Test, describe, fuzz, only, test, todo)


suite : Test
suite =
    describe "Minithesis"
        [ general
        , fuzzers
        , shrinkers
        ]


testMinithesis : String -> Fuzzer a -> (a -> Bool) -> TestResult a -> Test
testMinithesis name fuzzer userFn expectedResult =
    Test.fuzz Fuzz.int name <|
        \seed ->
            Minithesis.test "" fuzzer userFn
                |> Minithesis.run seed
                |> Tuple.second
                |> Expect.equal expectedResult


testMinithesisCanGenerate : String -> Fuzzer a -> a -> Test
testMinithesisCanGenerate name fuzzer value =
    testMinithesis ("Can generate " ++ name)
        fuzzer
        (\v -> v /= value)
        (FailsWith value)


testMinithesisCanGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
testMinithesisCanGenerateSatisfying name fuzzer predicate =
    Test.fuzz Fuzz.int name <|
        \seed ->
            Minithesis.test
                ("Can generate value satisfying: " ++ name)
                fuzzer
                (not << predicate)
                |> Minithesis.run seed
                |> Tuple.second
                |> Minithesis.isFailsWith
                |> Expect.true "Should fail with an example of the value satisfying the predicate"


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
        , describe "oneOfValues"
            [ testMinithesisCanGenerate "element of the list: 1" (F.oneOfValues [ 1, 42 ]) 1
            , testMinithesisCanGenerate "element of the list: 42" (F.oneOfValues [ 1, 42 ]) 42
            , testMinithesis "One value -> picks it"
                (F.oneOfValues [ 42 ])
                (\n -> n == 42)
                Passes
            , testMinithesis "Can't draw from empty"
                (F.oneOfValues [])
                (\n -> True)
                (Error Unsatisfiable)
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
             , testMinithesis "Can't draw from empty"
                (F.oneOf [])
                (\n -> True)
                (Error Unsatisfiable)
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
             , testMinithesis "Can't draw from empty"
                (F.frequencyValues [])
                (\n -> True)
                (Error Unsatisfiable)
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
             , testMinithesis "Can't draw from empty"
                (F.frequency [])
                (\n -> True)
                (Error Unsatisfiable)
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
                (F.listWith { minLength = Nothing, maxLength = Just 0 } F.unit)
                (\list -> List.isEmpty list)
                Passes
            , testMinithesis "single item list"
                (F.listWith { minLength = Just 1, maxLength = Just 1 } F.unit)
                (\list -> List.length list == 1)
                Passes
            , testMinithesis "one to three items"
                (F.listWith { minLength = Just 1, maxLength = Just 3 } F.unit)
                (\list ->
                    let
                        length =
                            List.length list
                    in
                    length >= 1 && length <= 3
                )
                Passes
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
            , testMinithesis "unsatisfiable"
                (F.uniqueListOfLength 5 (F.int 1 3))
                (\_ -> True)
                (Error Unsatisfiable)
            ]
        , describe "uniqueListWith"
            [ testMinithesis "empty list"
                (F.uniqueListWith { minLength = Nothing, maxLength = Just 0 } (F.int 1 1))
                (\list -> List.isEmpty list)
                Passes
            , testMinithesis "single item list"
                (F.uniqueListWith { minLength = Just 1, maxLength = Just 1 } (F.int 1 1))
                (\list -> List.length list == 1)
                Passes
            , testMinithesis "one to three items"
                (F.uniqueListWith { minLength = Just 1, maxLength = Just 3 } (F.int 1 10))
                (\list ->
                    let
                        length =
                            List.length list
                    in
                    length >= 1 && length <= 3
                )
                Passes
            , testMinithesis "if given wiggling space does what it can"
                (F.uniqueListWith { minLength = Just 1, maxLength = Nothing } (F.int 1 1))
                (\list -> List.length list == 1)
                Passes
            , testMinithesis "unsatisfiable"
                (F.uniqueListWith { minLength = Just 2, maxLength = Just 2 } (F.int 1 1))
                (\_ -> True)
                (Error Unsatisfiable)
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
