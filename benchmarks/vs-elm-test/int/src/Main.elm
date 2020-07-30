module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Expect
import Fuzz
import Minithesis
import Minithesis.Fuzz
import Random
import Test
import Test.Runner


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    let
        minithesisFuzzer =
            Minithesis.Fuzz.int 0 10000

        elmTestFuzzer =
            Fuzz.intRange 0 10000

        seed0 =
            Random.initialSeed 0

        minithesisTest () =
            -- implicit 100 examples
            Minithesis.run 0
                (Minithesis.test
                    "minithesis"
                    minithesisFuzzer
                    (\i -> i < 5000)
                )

        elmTestTest () =
            let
                runners =
                    Test.Runner.fromTest 100
                        seed0
                        (Test.fuzz
                            elmTestFuzzer
                            "elm-test"
                            (\i -> Expect.lessThan 5000 i)
                        )
            in
            case runners of
                Test.Runner.Plain xs ->
                    List.map (\runner -> runner.run ()) xs

                Test.Runner.Only xs ->
                    List.map (\runner -> runner.run ()) xs

                Test.Runner.Skipping xs ->
                    List.map (\runner -> runner.run ()) xs

                Test.Runner.Invalid str ->
                    [ [] ]

        minithesisGen () =
            Minithesis.Fuzz.generate minithesisFuzzer

        elmTestGen () =
            Test.Runner.fuzz elmTestFuzzer
                |> Result.map (\gen -> Random.step gen seed0)
    in
    describe "int 0 10000"
        [ Benchmark.compare "just generate a value"
            "minithesis"
            (\_ -> minithesisGen ())
            "elm-test"
            (\_ -> elmTestGen ())
        , Benchmark.compare "run a whole test"
            "minithesis"
            (\_ -> minithesisTest ())
            "elm-test"
            (\_ -> elmTestTest ())
        ]
