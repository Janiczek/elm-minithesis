module Minithesis.RandomRun exposing
    ( RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , get
    , isEmpty
    , length
    , replaceChunkWithZero
    , set
    )

import Array exposing (Array)


type alias RandomRun =
    Array Int


empty : RandomRun
empty =
    Array.empty


isEmpty : RandomRun -> Bool
isEmpty run =
    Array.isEmpty run


{-| Returns a key that can be used for the shrinking order of test cases.
-}
sortKey : RandomRun -> ( Int, List Int )
sortKey run =
    ( Array.length run
    , Array.toList run
    )


compare : RandomRun -> RandomRun -> Order
compare a b =
    Basics.compare (sortKey a) (sortKey b)


length : RandomRun -> Int
length run =
    Array.length run


get : Int -> RandomRun -> Maybe Int
get index run =
    Array.get index run


set : Int -> Int -> RandomRun -> RandomRun
set index value run =
    Array.set index value run


append : Int -> RandomRun -> RandomRun
append n run =
    Array.push n run


deleteChunk : { chunkSize : Int, startIndex : Int } -> RandomRun -> RandomRun
deleteChunk { chunkSize, startIndex } run =
    Array.append
        -- before
        (Array.slice 0 startIndex run)
        -- after
        (Array.slice (startIndex + chunkSize) (Array.length run) run)


replaceChunkWithZero : { chunkSize : Int, startIndex : Int } -> RandomRun -> RandomRun
replaceChunkWithZero { chunkSize, startIndex } run =
    Array.append
        (Array.append
            -- before
            (Array.slice 0 startIndex run)
            -- zeros
            (Array.repeat chunkSize 0)
        )
        -- after
        (Array.slice (startIndex + chunkSize) (Array.length run) run)
