module Main exposing (..)

import Expect
import Fuzz as F
import Minithesis.Fuzz as MF
import OurBenchmark exposing (ourBenchmark)


main =
    ourBenchmark
        { name = "char"
        , minithesisFuzzer = MF.char
        , elmTestFuzzer = F.char
        , minithesisFn = \c -> Char.isAlpha c
        , elmTestFn = \c -> Char.isAlpha c |> Expect.true ""
        }
