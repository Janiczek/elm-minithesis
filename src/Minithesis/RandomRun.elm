module Minithesis.RandomRun exposing
    ( RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , get
    , isEmpty
    , length
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapIfOutOfOrder
    , toList
    , update
    )

import Array exposing (Array)
import Array.Extra


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


replace : List ( Int, Int ) -> RandomRun -> RandomRun
replace replacements run =
    let
        runLength =
            Array.length run

        isDoable =
            List.all (\( i, _ ) -> i < runLength) replacements
    in
    if isDoable then
        List.foldl
            (\( i, value ) acc -> Array.set i value acc)
            run
            replacements

    else
        run


sortChunk : { chunkSize : Int, startIndex : Int } -> RandomRun -> RandomRun
sortChunk { chunkSize, startIndex } run =
    let
        chunk : Array Int
        chunk =
            Array.slice startIndex (startIndex + chunkSize) run

        sortedIndexedChunk : List ( Int, Int )
        sortedIndexedChunk =
            chunk
                |> Array.toList
                |> List.sort
                |> List.indexedMap (\i value -> ( startIndex + i, value ))
    in
    replace sortedIndexedChunk run


swapIfOutOfOrder :
    { leftIndex : Int, rightIndex : Int }
    -> RandomRun
    -> Maybe { newRun : RandomRun, newLeft : Int, newRight : Int }
swapIfOutOfOrder { leftIndex, rightIndex } run =
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replace
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
                        run
                , newLeft = right
                , newRight = left
                }

            else
                { newRun = run
                , newLeft = left
                , newRight = right
                }
        )
        (get leftIndex run)
        (get rightIndex run)


update : Int -> (Int -> Int) -> RandomRun -> RandomRun
update index fn run =
    Array.Extra.update index fn run


toList : RandomRun -> List Int
toList run =
    Array.toList run
