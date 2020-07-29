module Test.Minithesis exposing (mFuzz, mFuzzWith)

{-| Interop with `elm-explorations/test`

@docs mFuzz, mFuzzWith

-}

import Expect
import Fuzz
import Minithesis exposing (TestResult(..))
import Minithesis.TestingState exposing (Test(..))
import Random
import Shrink
import Test


seedFuzzer : Fuzz.Fuzzer { minithesisSeed : Int }
seedFuzzer =
    Fuzz.custom
        (Random.int 0 Random.maxInt
            |> Random.map (\seed -> { minithesisSeed = seed })
        )
        Shrink.noShrink


{-| Automatically sets elm-test fuzz options to `{ runs = 1 }`, since Minithesis
already has 100 runs as default.

(Keeping defaults of both would effectively make Minithesis try to generate
examples somewhere between 10k and 100k times per single test.)

-}
mFuzz : Minithesis.Test a -> Test.Test
mFuzz ((Test test) as wrappedTest) =
    Test.fuzzWith { runs = 1 } seedFuzzer test.label <|
        \{ minithesisSeed } ->
            Minithesis.run minithesisSeed wrappedTest
                |> Tuple.second
                |> Expect.equal Passes


{-| Automatically sets elm-test fuzz options to `{ runs = 1 }`, since Minithesis
already has 100 runs as default.

(Keeping defaults of both would effectively make Minithesis try to generate
examples somewhere between 10k and 100k times per single test.)

-}
mFuzzWith : Minithesis.Options -> Minithesis.Test a -> Test.Test
mFuzzWith options ((Test test) as wrappedTest) =
    Test.fuzzWith { runs = 1 } seedFuzzer test.label <|
        \{ minithesisSeed } ->
            Minithesis.runWith options minithesisSeed wrappedTest
                |> Tuple.second
                |> Expect.equal Passes
