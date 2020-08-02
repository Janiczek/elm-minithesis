module Minithesis.Fuzz.Example exposing (Examples(..), example, exampleWithRuns, exampleWithSeed, exampleWith)

{-|

@docs Examples, example, exampleWithRuns, exampleWithSeed, exampleWith

-}


{-| Examples can be returned either as-are, or with additional metadata (the
PRNG run).
-}
type Examples a
    = Examples (List a)
    | ExamplesWithRuns (List { example : a, randomRun : List Int })


{-| Shows 10 examples of randomly generated values from the given fuzzer, with
0 as the PRNG seed.

     Fuzz.example (Fuzz.uniqueList (Fuzz.int 1 3))
     -->
     [[1],[3],[],[3],[],[],[3],[3,2],[3],[1,3,2]]

-}
example : Fuzzer a -> Examples a
example fuzzer =
    exampleWith
        { seed = 0
        , showRun = False
        }
        fuzzer


{-| Shows 10 examples of randomly generated values from the given fuzzer, with
0 as the PRNG seed, also showing the PRNG choices made during generation.

     Fuzz.exampleWithRuns (Fuzz.uniqueList (Fuzz.int 1 3))
     -->
     [[1],[3],[],[3],[],[],[3],[3,2],[3],[1,3,2]]

-}
exampleWithRuns : Fuzzer a -> Examples a
exampleWithRuns fuzzer =
    exampleWith
        { seed = 0
        , showRun = True
        }
        fuzzer


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
exampleWithSeed : Int -> Fuzzer a -> Examples a
exampleWithSeed seed fuzzer =
    exampleWith
        { seed = seed
        , showRun = False
        }
        fuzzer


{-| Generic version of `example*` functions.

     Fuzz.exampleWith
        { seed = 0
        , showRun = True
        }
        (Fuzz.int 1 3)
     -->
     ExamplesWithRuns
        [ { example = 2, randomRun = [1] }
        , { example = 3, randomRun = [2] }
        , { example = 3, randomRun = [2] }
        , { example = 3, randomRun = [2] }
        , { example = 1, randomRun = [0] }
        , { example = 1, randomRun = [0] }
        , { example = 1, randomRun = [0] }
        , { example = 3, randomRun = [2] }
        , { example = 3, randomRun = [2] }
        , { example = 2, randomRun = [1] }
        ]

-}
exampleWith : { seed : Int, showRun : Bool } -> Fuzzer a -> Examples a
exampleWith options (Fuzzer fn) =
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

        go :
            Int
            -> Int
            -> Random.Seed
            -> List { example : a, randomRun : List Int }
            -> List { example : a, randomRun : List Int }
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
                            ({ example = value, randomRun = RandomRun.toList testCase.randomRun } :: acc)

                    Err ( _, testCase ) ->
                        go
                            (tries - 1)
                            i
                            (nextSeed seed testCase.seed)
                            acc
    in
    go 100 10 (Random.initialSeed options.seed) []
        |> (if options.showRun then
                ExamplesWithRuns

            else
                List.map .example >> Examples
           )
