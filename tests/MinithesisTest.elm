module MinithesisTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Minithesis exposing (TestResult(..))
import Minithesis.Fuzz as MFuzz exposing (Fuzzer)
import Minithesis.Stop exposing (Stop(..))
import Set
import Test


suite : Test.Test
suite =
    Test.describe "Minithesis"
        [ Test.describe "Fuzz.filter"
            [ testMinithesis "Stops with Unsatisfiable if rejecting too many values"
                -- this rejects everything:
                (MFuzz.int 0 10 |> MFuzz.filter (\_ -> False))
                -- this will never run:
                (\_ -> True)
                (Error Unsatisfiable)
            , testMinithesis "Generated values satisfy preconditions"
                -- this rejects 0:
                (MFuzz.int 0 10 |> MFuzz.filter (\n -> n /= 0))
                -- this expects no 0:
                (\n -> n /= 0)
                -- so it should be OK:
                Passes
            , testMinithesis "Another example"
                -- this rejects odd values:
                (MFuzz.int 0 10 |> MFuzz.filter (\n -> modBy 2 n == 0))
                -- this expects no odd values:
                (\n -> modBy 2 n == 0)
                -- so it should be OK:
                Passes
            ]
        , Test.describe "Fuzz.reject"
            [ testMinithesis "Makes the test never run"
                MFuzz.reject
                -- this will never run:
                (\n -> True)
                (Error Unsatisfiable)
            ]
        , Test.describe "Fuzz.map"
            [ testMinithesis "Even numbers"
                (MFuzz.int 0 5 |> MFuzz.map (\n -> n * 2))
                (\n -> modBy 2 n == 0)
                Passes
            ]
        , Test.describe "Fuzz.int"
            [ testMinithesis "Can work with negative numbers"
                (MFuzz.int -10 10)
                (\n -> n >= -10 && n <= 10)
                Passes
            , testMinithesis "Random.minInt"
                (MFuzz.int -2147483648 -2147483648)
                (\n -> n == -2147483648)
                Passes
            , testMinithesis "Random.maxInt"
                (MFuzz.int 2147483647 2147483647)
                (\n -> n == 2147483647)
                Passes
            , testMinithesis "Full range"
                (MFuzz.int -2147483648 2147483647)
                (\n -> n >= -2147483648 && n <= 2147483647)
                Passes
            ]
        , Test.describe "Fuzz.anyInt"
            [ testMinithesis "Full range"
                MFuzz.anyInt
                (\n -> n >= -2147483648 && n <= 2147483647)
                Passes
            ]
        , Test.describe "Fuzz.oneOfValues"
            [ testMinithesis "Draws from all"
                (MFuzz.oneOfValues [ 1, 42 ])
                (\n -> n == 1 || n == 42)
                Passes
            , testMinithesis "Can't draw from empty"
                (MFuzz.oneOfValues [])
                -- this will never run:
                (\n -> True)
                (Error Unsatisfiable)
            ]
        , Test.describe "Fuzz.oneOf"
            [ testMinithesis "Draws from all"
                -- possible values: -5 -4 -3 -2 -1 0 ___ 2 3 4 5: purposefully missing 1
                (MFuzz.oneOf
                    [ MFuzz.int -5 0
                    , MFuzz.int 2 5
                    ]
                )
                (\n -> n <= 5 && n >= -5 && n /= 1)
                Passes
            , testMinithesis "Can't draw from empty"
                (MFuzz.oneOf [])
                -- this will never run:
                (\n -> True)
                (Error Unsatisfiable)
            ]
        , Test.describe "Fuzz.andThen"
            [ testMinithesis "Works :shrug:"
                (MFuzz.int 0 5
                    |> MFuzz.andThen
                        (\m ->
                            MFuzz.tuple
                                ( MFuzz.constant m
                                , MFuzz.int m (m + 10)
                                )
                        )
                )
                (\( m, n ) -> m <= n && n <= m + 10)
                Passes
            ]
        , Test.describe "weightedBool"
            [ testMinithesis "Always False"
                (MFuzz.weightedBool 0)
                (\bool -> not bool)
                Passes
            , testMinithesis "Always True"
                (MFuzz.weightedBool 1)
                (\bool -> bool)
                Passes
            ]
        , Test.describe "listWith"
            [ testMinithesis "empty list"
                (MFuzz.listWith { minLength = Nothing, maxLength = Just 0 } MFuzz.unit)
                (\list -> List.isEmpty list)
                Passes
            , testMinithesis "single item list"
                (MFuzz.listWith { minLength = Just 1, maxLength = Just 1 } MFuzz.unit)
                (\list -> List.length list == 1)
                Passes
            , testMinithesis "one to three items"
                (MFuzz.listWith { minLength = Just 1, maxLength = Just 3 } MFuzz.unit)
                (\list ->
                    let
                        length =
                            List.length list
                    in
                    length >= 1 && length <= 3
                )
                Passes
            ]
        , Test.describe "listOfLength"
            [ testMinithesis "always the specified length"
                (MFuzz.int 0 10
                    |> MFuzz.andThen
                        (\length ->
                            MFuzz.tuple
                                ( MFuzz.constant length
                                , MFuzz.listOfLength length MFuzz.unit
                                )
                        )
                )
                (\( length, list ) -> List.length list == length)
                Passes
            ]
        , Test.describe "uniqueList"
            [ testMinithesis "elements are unique"
                (MFuzz.uniqueList MFuzz.anyInt)
                (\list -> Set.size (Set.fromList list) == List.length list)
                Passes
            ]
        , Test.describe "uniqueListOfLength"
            [ testMinithesis "always the specified length"
                (MFuzz.int 0 10
                    |> MFuzz.andThen
                        (\length ->
                            MFuzz.tuple
                                ( MFuzz.constant length
                                , MFuzz.uniqueListOfLength length MFuzz.anyInt
                                )
                        )
                )
                (\( length, list ) -> List.length list == length)
                Passes
            , testMinithesis "unsatisfiable"
                (MFuzz.uniqueListOfLength 5 (MFuzz.int 1 3))
                (\_ -> True)
                (Error Unsatisfiable)
            ]
        , Test.describe "shrinkers"
            [ testMinithesis "Reduces additive pairs"
                (MFuzz.tuple
                    ( MFuzz.int 0 1000
                    , MFuzz.int 0 1000
                    )
                )
                (\( m, n ) -> m + n <= 1000)
                (FailsWith ( 1, 1000 ))
            ]
        ]


testMinithesis : String -> Fuzzer a -> (a -> Bool) -> TestResult a -> Test.Test
testMinithesis name fuzzer userFn expectedResult =
    Test.fuzz Fuzz.int name <|
        \seed ->
            Minithesis.test "" fuzzer userFn
                |> Minithesis.run seed
                |> Tuple.second
                |> Expect.equal expectedResult
