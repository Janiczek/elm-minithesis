module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Minithesis as M
import Minithesis.Fuzz as F exposing (Fuzzer)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    let
        generate100Times fuzzer =
            M.run 0 (M.test "" fuzzer (\_ -> True))

        generateAndShrinkFirstFailure fuzzer =
            M.run 0 (M.test "" fuzzer (\str -> String.length str < 5))
    in
    describe "Minithesis"
        [ describe "less empty string - 100 times, no shrinking"
            [ benchmark "map3" <|
                \_ -> generate100Times map3Fuzzer
            , benchmark "frequency" <|
                \_ -> generate100Times frequencyFuzzer
            , benchmark "andThen" <|
                \_ -> generate100Times andThenFuzzer
            , benchmark "stringWith" <|
                \_ -> generate100Times stringWithFuzzer
            ]
        , describe "less empty string - until first failure + shrink"
            [ benchmark "map3" <|
                \_ -> generateAndShrinkFirstFailure map3Fuzzer
            , benchmark "frequency" <|
                \_ -> generateAndShrinkFirstFailure frequencyFuzzer
            , benchmark "andThen" <|
                \_ -> generateAndShrinkFirstFailure andThenFuzzer
            , benchmark "stringWith" <|
                \_ -> generateAndShrinkFirstFailure stringWithFuzzer
            ]
        ]


map3Fuzzer : Fuzzer String
map3Fuzzer =
    F.map3
        (\s1 s2 s3 ->
            if String.isEmpty s1 then
                if String.isEmpty s2 then
                    s3

                else
                    s2

            else
                s1
        )
        F.string
        F.string
        F.string


andThenFuzzer : Fuzzer String
andThenFuzzer =
    F.string
        |> F.andThen
            (\s1 ->
                if String.isEmpty s1 then
                    F.string
                        |> F.andThen
                            (\s2 ->
                                if String.isEmpty s2 then
                                    F.string

                                else
                                    F.constant s2
                            )

                else
                    F.constant s1
            )


frequencyFuzzer : Fuzzer String
frequencyFuzzer =
    F.frequency
        [ ( 2
          , F.stringWith
                { minLength = Just 1
                , maxLength = Nothing
                , customAverageLength = Nothing
                , charFuzzer = F.char
                }
          )
        , ( 1, F.constant "" )
        ]


stringWithFuzzer : Fuzzer String
stringWithFuzzer =
    F.stringWith
        -- just a guess
        { minLength = Nothing
        , maxLength = Nothing
        , customAverageLength = Just 3
        , charFuzzer = F.char
        }
