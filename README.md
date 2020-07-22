# `elm-minithesis`

This is an Elm port of [Minithesis](https://github.com/drmaciver/minithesis),
the minimal implementation of the core idea of
[Hypothesis](https://github.com/HypothesisWorks/hypothesis).

Hypothesis itself is a Python testing library for property-based testing. What
sets it apart is its underlying implementation: instead of working on the
generated values themselves (defining shrinkers on these values), it remembers
the underlying random "dice rolls" that were used to generate the values,
and it shrinks *those*. 

```elm
-- QuickCheck and most other property-based testing libraries
shrink : a -> LazyList a

-- Hypothesis
shrink : List Int -> List (List Int)
```

A very cool consequence of the above is is that it mostly sidesteps the issue
most other property-based testing libraries have: `andThen` (monadic bind). More
specifically, QuickCheck-like libraries either don't expose `andThen` at all (as
is the case with Elm), or struggle with making the shrunk values satisfy the
same invariants the `andThen`-generated values do. Hypothesis instead shrinks
the underlying "dice roll" history and generates a new value from that, so
its values satisfy the invariants out of the box even if using `andThen`!

(Source: ["Integrated vs type based shrinking"](https://hypothesis.works/articles/integrated-shrinking/))

## Usage

`tests/Example.elm` is a pretty nice example of how the API looks.
Here is an abridged version:

```elm
import Minithesis.Fuzz as Fuzz exposing (Fuzzer)
import Minithesis exposing (Test, TestResult)

listOfIntegers : Fuzzer (List Int)
listOfIntegers =
    let
        go : List Int -> Fuzzer (List Int)
        go acc =
            Fuzz.weightedBool 0.9
                |> Fuzz.andThen
                    (\coin ->
                        if coin then
                            Fuzz.nonnegativeIntFromTo 1001 10000
                                |> Fuzz.andThen (\int -> go (int :: acc))

                        else
                            Fuzz.constant acc
                    )
    in
    go []


findsSmallList : Test (List Int)
findsSmallList =
    Minithesis.test listOfIntegers <|
        \fuzzedList ->
            List.sum fuzzedList <= 1000


{-| Will fail and shrink to the minimal example: `FailsWith [1001]`
-}
minithesisTestResult : Int -> TestResult (List Int)
minithesisTestResult seed =
    Minithesis.run seed findsSmallList
```
