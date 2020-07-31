module Minithesis.TestCase exposing
    ( Status(..)
    , TestCase
    , defaultBufferSize
    , forRun
    , init
    , isInteresting
    , markStatus
    )

import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Minithesis.Stop exposing (Stop(..))
import Random


type Status
    = Undecided
    | Overrun -- Test case didn't have enough data to complete
    | Invalid { rejection : String } -- Test case contained something that prevented completion
    | Valid -- Test case completed just fine but was boring (typically passed test)
    | Interesting -- Test case completed and was interesting (typically didn't pass test)


type alias TestCase =
    { seed : Maybe Random.Seed
    , maxSize : Int
    , status : Status
    , prefix : RandomRun
    , randomRun : RandomRun
    }


init :
    { seed : Random.Seed
    , maxSize : Int
    , prefix : RandomRun
    }
    -> TestCase
init options =
    { seed = Just options.seed
    , maxSize = options.maxSize
    , status = Undecided
    , prefix = options.prefix
    , randomRun = RandomRun.empty
    }


forRun : RandomRun -> TestCase
forRun run =
    { seed = Nothing
    , maxSize = RandomRun.length run
    , status = Undecided
    , prefix = run
    , randomRun = RandomRun.empty
    }


markStatus : Status -> TestCase -> Result ( Stop, TestCase ) x
markStatus status testCase =
    if testCase.status /= Undecided then
        Err ( Frozen, testCase )

    else
        Err
            ( StopTest
            , { testCase | status = status }
            )


isInteresting : TestCase -> Bool
isInteresting { status } =
    status == Interesting


{-| We cap the maximum amount of entropy a test case can use. This prevents
cases where the generated test case size explodes by effectively rejection.
-}
defaultBufferSize : Int
defaultBufferSize =
    8 * 1024
