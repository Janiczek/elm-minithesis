module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Minithesis
import Minithesis.Fuzz
import Random


main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    Benchmark.describe "Minithesis"
        [ {- benchmark_ "bool"
                 Minithesis.Fuzz.bool
             , benchmark_ "int 0 10"
                 (Minithesis.Fuzz.int 0 10)
             , benchmark_ "weightedBool 0.75"
                 (Minithesis.Fuzz.weightedBool 0.75)
             , benchmark_ "unit"
                 Minithesis.Fuzz.unit
             , benchmark_ "triple ints"
                 (Minithesis.Fuzz.tuple3
                     (Minithesis.Fuzz.int 0 10)
                     (Minithesis.Fuzz.int 0 10)
                     (Minithesis.Fuzz.int 0 10)
                     )
             ,
          -}
          benchmark_ "list of ints"
            (Minithesis.Fuzz.list (Minithesis.Fuzz.int 0 10))
        ]


benchmark_ :
    String
    -> Minithesis.Fuzz.Fuzzer a
    -> Benchmark
benchmark_ label fuzzer =
    let
        genAndShrink seed () =
            Minithesis.runWith
                { maxExamples = 100
                , showShrinkHistory = False
                }
                seed
                (Minithesis.test
                    label
                    fuzzer
                    (always False)
                )

        gen seed () =
            Minithesis.Fuzz.generateWithSeed seed fuzzer
    in
    describe label
        [ benchmark "(0) gen" (gen (Random.initialSeed 0))
        , benchmark "(1) gen" (gen (Random.initialSeed 1))
        , benchmark "(2) gen" (gen (Random.initialSeed 2))
        , benchmark "(3) gen" (gen (Random.initialSeed 3))
        , benchmark "(4) gen" (gen (Random.initialSeed 4))
        , benchmark "(0) gen + shrink" (genAndShrink 0)
        , benchmark "(1) gen + shrink" (genAndShrink 1)
        , benchmark "(2) gen + shrink" (genAndShrink 2)
        , benchmark "(3) gen + shrink" (genAndShrink 3)
        , benchmark "(4) gen + shrink" (genAndShrink 4)
        ]
