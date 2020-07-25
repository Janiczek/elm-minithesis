module OurExtras.Bitwise exposing
    ( combineTo52BitNumber
    , isBitSet
    , ones
    , keepBits
    , reverseNBits
    , reverse52Bits
    )

import Bitwise


isBitSet : Int -> Int -> Bool
isBitSet index num =
    (num
        |> Bitwise.shiftRightBy index
        |> Bitwise.and 1
    )
        == 1


ones : Int -> Int
ones count =
    Bitwise.shiftLeftBy count 1 - 1


keepBits : Int -> Int -> Int
keepBits count num =
    Bitwise.and (ones count) num


combineTo52BitNumber : (Int, Int) -> Int
combineTo52BitNumber (highBits, lowBits) =
    Bitwise.or
        (highBits |> keepBits 20 |> Bitwise.shiftLeftBy 32)
        (lowBits |> keepBits 32)


reverseNBits : Int -> Int -> Int
reverseNBits n bits =
  let
    go : Int -> Int -> Int -> Int
    go b acc i =
      if i <= 0 then
        acc

      else
        go
          (Bitwise.shiftRightBy 1 b)
          (acc
            |> Bitwise.shiftLeftBy 1
            |> Bitwise.or (Bitwise.and 1 b)
          )
          (i - 1)
  in
  go bits 0 n


reverse52Bits : (Int, Int) -> (Int, Int)
reverse52Bits (high20, low32) =
  {- TODO in Hypothesis this uses optimized variants of `reverseNBits` for these
     non-dynamic sizes (in Hypothesis 64, here we could use 32 and 20). Probably
     let's first see how's the performance before we start optimizing with a
     memoized/tabled approach.
  -}
  let
    reversedLow32 =
      reverseNBits 32 low32

    reversedHigh20 =
      reverseNBits 20 high20

    newHigh20 =
      reversedLow32
        |> Bitwise.shiftRightBy 12
        |> keepBits 20

    newLow32 =
      reversedLow32
        |> keepBits 12
        |> Bitwise.shiftLeftBy 20
        |> Bitwise.or reversedHigh20
  in
  (newHigh20, newLow32)
