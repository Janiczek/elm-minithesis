module Main exposing (..)

import Expect
import Fuzz as F
import Minithesis.Fuzz as MF
import OurBenchmark exposing (ourBenchmark)


main =
    ourBenchmark
        { name = "float 0 10000"
        , minithesisFuzzer = MF.float 0 10000
        , elmTestFuzzer = F.floatRange 0 10000
        , minithesisFn = \f -> f < 5000
        , elmTestFn = \f -> Expect.lessThan 5000 f
        }
