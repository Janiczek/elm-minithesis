module Minithesis.TestCase exposing
    ( Status(..)
    , Stop(..)
    , TestCase
    , forRun
    , init
    , markStatus
    )

import Minithesis.RandomRun as RandomRun exposing (RandomRun)
import Random


type Stop
    = Frozen -- Attempted to make random choices on a test case that has been completed
    | StopTest -- Raised when a test should stop executing early
    | InvalidChoice Int
    | PrefixNotLongEnough -- shouldn't happen as we're checking the lenghts beforehand
    | RandomnessNotAllowed -- seed was Nothing but was requested to generate a value
    | LostCounterexample -- BUG: somehow TestingState.bestCounterexample became Nothing


type Status
    = Undecided
    | Overrun -- Test case didn't have enough data to complete
    | Invalid -- Test case contained something that prevented completion
    | Valid -- Test case completed just fine but was boring
    | Interesting -- Test case completed and was interesting


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
