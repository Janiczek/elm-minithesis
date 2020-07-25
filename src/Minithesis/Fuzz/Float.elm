module Minithesis.Fuzz.Float exposing (lexToFloat)

import Bitwise
import OurExtras.Bitwise as Bitwise
import Dict exposing (Dict)


lexToFloat : (Int, Int) -> Float
lexToFloat (highBits, lowBits) =
    let
        hasFractionalPart : Bool
        hasFractionalPart =
            Bitwise.isBitSet 63 highBits
    in
    if hasFractionalPart then
        let
            rawExponent =
              highBits
                |> Bitwise.shiftRightBy 20
                |> Bitwise.keepBits 11

            exponent =
              decodeExponent rawExponent

            rawMantissaHigh =
              Bitwise.keepBits 20 highBits

            rawMantissaLow =
              lowBits

            (mantissaHigh, mantissaLow) =
              updateMantissa (exponent - bias) (rawMantissaHigh, rawMantissaLow)

            mantissaBits =
              bitsToList (mantissaHigh, mantissaLow)
        in
        floatFromParts
          { mantissaBits = mantissaBits
          , unbiasedExponent = exponent - bias
          }

    else
        (highBits, lowBits)
        |> Bitwise.combineTo52BitNumber
        |> toFloat


maxExponent : Int
maxExponent = 0x7FFF


bias : Int
bias = 1023


fraction : List (Int, Int) -> Float
fraction bits =
  List.foldl
    (\(i, bit) acc -> acc + toFloat bit * 2^(negate (toFloat i)))
    0
    bits


floatFromParts :
  { mantissaBits : List (Int, Int)
  , unbiasedExponent : Int
  }
    -> Float
floatFromParts { mantissaBits, unbiasedExponent } =
  (1 + fraction mantissaBits) * 2^(toFloat unbiasedExponent)


bitsToList : (Int, Int) -> List (Int, Int)
bitsToList (high, low) =
  let
    go : Int -> Int -> Int -> List (Int, Int) -> List (Int, Int)
    go howMany startAt bits acc =
      if howMany <= 0 then
        acc

      else
        go
          (howMany - 1)
          startAt
          (Bitwise.shiftRightBy 1 bits)
          ((startAt + howMany, Bitwise.and 1 bits) :: acc)
  in
  go 20 0 high [] ++ go 32 20 low []


decodeExponent : Int -> Int
decodeExponent e =
  Dict.get e exponentMapping
    |> Maybe.withDefault 0


exponentMapping : Dict Int Int
exponentMapping =
  List.range 0 maxExponent
    |> List.sortBy exponentKey
    |> List.indexedMap Tuple.pair
    |> Dict.fromList


exponentKey : Int -> Int
exponentKey e =
  if e == maxExponent then
    round (1/0)

  else
    let
      unbiased = e - bias
    in
    if unbiased < 0 then
      10000 - unbiased

    else
      unbiased


updateMantissa : Int -> (Int, Int) -> (Int, Int)
updateMantissa unbiasedExponent (mantissaHigh, mantissaLow) =
  if unbiasedExponent <= 0 then
    Bitwise.reverse52Bits (mantissaHigh, mantissaLow)

  else if unbiasedExponent <= 51 then
    let
      nFractionalBits =
        52 - unbiasedExponent

      fractionalPartHigh =
        if nFractionalBits >= 32 then
          Bitwise.keepBits (nFractionalBits - 32) mantissaHigh

        else
          0

      fractionalPartLow =
        if nFractionalBits < 32 then
          Bitwise.keepBits nFractionalBits mantissaLow

        else
          mantissaLow

      reversedHigh =
          if nFractionalBits >= 32 then
            Bitwise.reverseNBits (nFractionalBits - 32) fractionalPartHigh

          else
            0

      reversedLow =
        if nFractionalBits < 32 then
          Bitwise.reverseNBits nFractionalBits mantissaLow

        else
          Bitwise.reverseNBits 32 fractionalPartLow

      newMantissaHigh =
        mantissaHigh
          |> Bitwise.xor fractionalPartHigh
          |> Bitwise.or reversedHigh

      newMantissaLow =
        mantissaLow
          |> Bitwise.xor fractionalPartLow
          |> Bitwise.or reversedLow
    in
      (newMantissaHigh, newMantissaLow)

  else
    (mantissaHigh, mantissaLow)
