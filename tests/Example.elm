module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Minithesis
import Minithesis.Fuzz
import Test


listOfIntegers : Minithesis.Fuzz.Fuzzer (List Int)
listOfIntegers =
    let
        go : List Int -> Minithesis.Fuzz.Fuzzer (List Int)
        go acc =
            Minithesis.Fuzz.weightedBool 0.9
                |> Minithesis.Fuzz.andThen
                    (\coin ->
                        if coin then
                            Minithesis.Fuzz.nonnegativeIntFromTo 1001 10000
                                |> Minithesis.Fuzz.andThen (\int -> go (int :: acc))

                        else
                            Minithesis.Fuzz.constant acc
                    )
    in
    go []


findsSmallList : Minithesis.Test (List Int)
findsSmallList =
    Minithesis.test listOfIntegers <|
        \fuzzedList ->
            List.sum fuzzedList <= 1000


minithesisTestResult : Int -> Minithesis.TestResult (List Int)
minithesisTestResult seed =
    Minithesis.run seed findsSmallList


suite : Test.Test
suite =
    Test.fuzz Fuzz.int "Finds an counterexample and successfully shrinks it" <|
        \seed ->
            minithesisTestResult seed
                |> Expect.equal (Minithesis.FailsWith [ 1001 ])
