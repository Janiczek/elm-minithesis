module Minithesis.Stop exposing (Stop(..))


type Stop
    = Frozen -- Attempted to make random choices on a test case that has been completed
    | StopTest -- Raised when a test should stop executing early
    | Unsatisfiable -- Raised when test has no valid examples
    | InvalidChoice Int
    | PrefixNotLongEnough -- shouldn't happen as we're checking the lenghts beforehand
    | RandomnessNotAllowed -- seed was Nothing but was requested to generate a value
    | LostCounterexample -- BUG: somehow TestingState.bestCounterexample became Nothing
