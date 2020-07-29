# `elm-minithesis`

This is an Elm port of [Minithesis](https://github.com/drmaciver/minithesis),
the minimal implementation of the core idea of
[Hypothesis](https://github.com/HypothesisWorks/hypothesis).

> See more in the [About](#About) section.

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

### Examples

Try `Fuzz.example` and `Fuzz.exampleWithSeed` in the REPL for quick
sanity checks of your fuzzers! 

```elm
import Minithesis.Fuzz as F

F.string |> F.exampleWithSeed 0
-- gives a list of examples:
--> ["x","I","","6a=U",";W?","uDc",":ei_^~","=Y","-NAT\\QJ","{92H2DI}-(KOc"]

F.string |> F.exampleWithSeed 1
-- gives another list of examples:
--> ["KI","<j XT","","'xpvdQ1ONkM/","tdVd_v","I3=:e0i3","","P)y8$e@^y}1s",",]uz\\","8"]
```

## Inspect shrink history

Use the `showShrinkHistory` field of `Minithesis.runWith` to get additional
information about how shrinking of your data went. This gives a bit of
visibility into what happens in the black box that Minithesis shrinking is. 


```elm
import Minithesis as M
import Minithesis.Fuzz as F

M.runWith 
  { maxExamples = 100
  , showShrinkHistory = True 
  } 
  1
  (M.test "strings never contain 'x'"
    F.string
    (\string -> not (String.contains "x" string))
  )
  
( "strings never contain 'x'"
, FailsWithShrinks 
    { finalRun = [1,88,0]
    , finalValue = "x"
    , history = 
        [ { run = [1,7,1,88,1,80,1,86,1,68,1,49,1,17,1,47,1,46,1,75,1,45,1,15,0], shrinkerUsed = "Initial",                                                            value = "'xpvdQ1ONkM/" }
        , { run = [1,7,1,88,1,80,1,86,1,68,1,49,1,17,1,47,0],                     shrinkerUsed = "DeleteChunkAndMaybeDecrementPrevious { size = 8, startIndex = 17 }", value = "'xpvdQ1O"     }
        , { run = [1,7,1,88,1,80,1,86,0],                                         shrinkerUsed = "DeleteChunkAndMaybeDecrementPrevious { size = 8, startIndex = 9 }",  value = "'xpv"         }
        , { run = [1,7,1,88,0],                                                   shrinkerUsed = "DeleteChunkAndMaybeDecrementPrevious { size = 4, startIndex = 5 }",  value = "'x"           }
        , { run = [1,88,0],                                                       shrinkerUsed = "DeleteChunkAndMaybeDecrementPrevious { size = 2, startIndex = 1 }",  value = "x"            }
        ]
    }
)
```

Paired with some knowledge about which shrinking strategies there are and what
they do, you can sometimes tweak your fuzzers to optimize how they interact with
the shrinking process, allowing them to be shrunk better.

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
