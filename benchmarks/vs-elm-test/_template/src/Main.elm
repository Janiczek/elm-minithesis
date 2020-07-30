module Main exposing (..)

import Expect
import Fuzz as F
import Minithesis.Fuzz as MF
import OurBenchmark exposing (ourBenchmark)


main =
    ourBenchmark
        { name = "int 0 10000"
        , minithesisFuzzer = MF.int 0 10000
        , elmTestFuzzer = F.intRange 0 10000
        , minithesisFn = \i -> i < 5000
        , elmTestFn = \i -> Expect.lessThan 5000 i
        }
