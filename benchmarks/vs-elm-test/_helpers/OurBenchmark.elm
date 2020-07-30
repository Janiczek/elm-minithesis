module OurBenchmark exposing (ourBenchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Expect exposing (Expectation)
import Fuzz
import Minithesis
import Minithesis.Fuzz
import Random
import Test
import Test.Runner


type alias Options a =
    { name : String
    , minithesisFuzzer : Minithesis.Fuzz.Fuzzer a
    , elmTestFuzzer : Fuzz.Fuzzer a
    , minithesisFn : a -> Bool
    , elmTestFn : a -> Expectation
    }


ourBenchmark : Options a -> BenchmarkProgram
ourBenchmark options =
    Benchmark.Runner.program (suite options)


suite : Options a -> Benchmark
suite options =
    let
        seed0 =
            Random.initialSeed 0

        minithesisTest () =
            -- implicit 100 examples
            Minithesis.runWith
                { maxExamples = 100
                , showShrinkHistory = False
                }
                0
                (Minithesis.test
                    "minithesis"
                    options.minithesisFuzzer
                    options.minithesisFn
                )

        elmTestTest () =
            let
                runners =
                    Test.Runner.fromTest 100
                        seed0
                        (Test.fuzz
                            options.elmTestFuzzer
                            "elm-test"
                            options.elmTestFn
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
            Minithesis.Fuzz.generate options.minithesisFuzzer

        elmTestGen () =
            Test.Runner.fuzz options.elmTestFuzzer
                |> Result.map (\gen -> Random.step gen seed0)
    in
    describe options.name
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
