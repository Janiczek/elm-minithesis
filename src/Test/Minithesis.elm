module Test.Minithesis exposing (mFuzz, mFuzzWith)

{-| Interop with `elm-explorations/test`.

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


{-|

    import Test.Minithesis exposing (mFuzz)
    import Minithesis
    import Minithesis.Fuzz as MFuzz

    mFuzz <|
        Minithesis.test "list equals itself reversed"
            (MFuzz.list (MFuzz.int 1 10))
            (\list -> list == List.reverse list)

-}
mFuzz : Minithesis.Test a -> Test.Test
mFuzz ((Test test) as wrappedTest) =
    Test.fuzz seedFuzzer test.label <|
        \{ minithesisSeed } ->
            Minithesis.run minithesisSeed wrappedTest
                |> Tuple.second
                |> Expect.equal Passes


{-|

    import Test.Minithesis exposing (mFuzz)
    import Minithesis
    import Minithesis.Fuzz as MFuzz

    mFuzzWith
        { maxExamples = 100
        , showShrinkHistory = True
        }
            <|
        Minithesis.test "list equals itself reversed"
            (MFuzz.list (MFuzz.int 1 10))
            (\list -> list == List.reverse list)

-}
mFuzzWith : Minithesis.Options -> Minithesis.Test a -> Test.Test
mFuzzWith options ((Test test) as wrappedTest) =
    Test.fuzz seedFuzzer test.label <|
        \{ minithesisSeed } ->
            Minithesis.runWith options minithesisSeed wrappedTest
                |> Tuple.second
                |> Expect.equal Passes
