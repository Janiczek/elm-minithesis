module Main exposing (..)

import Expect
import Fuzz as F
import Minithesis.Fuzz as MF
import OurBenchmark exposing (ourBenchmark)


main =
    ourBenchmark
        { name = "anyNumericInt"
        , minithesisFuzzer = MF.anyNumericInt
        , elmTestFuzzer = F.int
        , minithesisFn = \i -> i > 10
        , elmTestFn = \i -> Expect.greaterThan 10 i
        }
