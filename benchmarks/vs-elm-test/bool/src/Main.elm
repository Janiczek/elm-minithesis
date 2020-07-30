module Main exposing (..)

import Expect
import Fuzz as F
import Minithesis.Fuzz as MF
import OurBenchmark exposing (ourBenchmark)


main =
    ourBenchmark
        { name = "bool"
        , minithesisFuzzer = MF.bool
        , elmTestFuzzer = F.bool
        , minithesisFn = \b -> not b
        , elmTestFn = \b -> Expect.false "" b
        }
