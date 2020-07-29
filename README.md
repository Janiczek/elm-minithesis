# `elm-minithesis`

```elm
import Minithesis.Fuzz as Fuzz exposing (Fuzzer)
import Minithesis exposing (Test, TestResult)

listOfIntegers : Fuzzer (List Int)
listOfIntegers =
    Fuzz.list (Fuzz.int 0 10000)


findsSmallList : Test (List Int)
findsSmallList =
    Minithesis.test "My awesome test" listOfIntegers <|
        \fuzzedList ->
            List.sum fuzzedList <= 1000


{-| Will fail and shrink to the minimal example: `( "My awesome test", FailsWith [ 1001 ] )`
-}
minithesisTestResult : Int -> TestResult (List Int)
minithesisTestResult seed =
    Minithesis.run seed findsSmallList
```

## Tips and tricks

* Try `Minithesis.example` and `Minithesis.exampleWithSeed` in the REPL for
  quick sanity checks of your fuzzers! 

```elm
> myFuzzer |> Fuzz.exampleWithSeed 0

TODO finish writing
```


## About

This is an Elm port of [Minithesis](https://github.com/drmaciver/minithesis),
the minimal implementation of the core idea of
[Hypothesis](https://github.com/HypothesisWorks/hypothesis).

Hypothesis itself is a Python testing library for property-based testing. What
sets it apart is its underlying implementation: instead of working on the
generated values themselves (defining shrinkers on these values), it remembers
the underlying random "dice rolls" that were used to generate the values,
and it shrinks *those*. 

```elm
-- QuickCheck and most other property-based testing libraries, including elm-test
shrink : a -> LazyList a -- has to be defined for each fuzzed type, doesn't shrink by default

-- Hypothesis
shrink : List Int -> List (List Int) -- shrinks all fuzzers automatically, can't be configured
```

A very cool consequence of the above is is that it mostly sidesteps the issue
most other property-based testing libraries have: `andThen` (monadic bind). More
specifically, QuickCheck-like libraries either don't expose `andThen` at all (as
is the case with Elm), or struggle with making the shrunk values satisfy the
same invariants the `andThen`-generated values do. Hypothesis instead shrinks
the underlying "dice roll" history and generates a new value from that, so
its values satisfy the invariants out of the box even if using `andThen`!

(Source: ["Integrated vs type based shrinking"](https://hypothesis.works/articles/integrated-shrinking/))

## Community

There is a small piece of internet dedicated to `elm-minithesis`: [the
`#elm-minithesis` channel on Incremental Elm
Discord](https://discord.gg/PC7Ckpg). Come join and hear about updates first!

The original discussion around `elm-minithesis` happened on the [Elm
Discourse](https://discourse.elm-lang.org/t/elm-minithesis-shrinking-without-compromises/6071/)
