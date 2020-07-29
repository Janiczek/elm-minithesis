module Minithesis.Fuzz.Float exposing
    ( fractionalFloat
    , lexToFloat
    , nastyFloats
    )

import Bitwise
import Dict exposing (Dict)
import OurExtras.Bitwise as Bitwise
import OurExtras.List as List


lexToFloat : ( Int, Int ) -> Float
lexToFloat ( highBits, lowBits ) =
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

            ( mantissaHigh, mantissaLow ) =
                updateMantissa (exponent - bias) ( rawMantissaHigh, rawMantissaLow )

            mantissaBits =
                bitsToList ( mantissaHigh, mantissaLow )
        in
        {- This can generate infinities according to the spec, but not NaNs:
           exponent 1024 + first bit of mantissa 0 == Infinity
           exponent 1024 + first bit of mantissa 1 should be NaN
                                              but instead == Infinity
        -}
        floatFromParts
            { mantissaBits = mantissaBits
            , unbiasedExponent = exponent - bias
            }

    else
        ( highBits, lowBits )
            |> Bitwise.combineTo52BitNumber
            |> toFloat


fractionalFloat : ( Int, Int ) -> Float
fractionalFloat ( high, low ) =
    fraction (bitsToList ( high, low ))
        / maxMantissa


maxMantissa : Float
maxMantissa =
    fraction (bitsToList ( 0x000FFFFF, 0xFFFFFFFF ))


maxExponent : Int
maxExponent =
    0x7FFF


bias : Int
bias =
    1023


fraction : List ( Int, Int ) -> Float
fraction bits =
    List.foldl
        (\( i, bit ) acc -> acc + toFloat bit * 2 ^ negate (toFloat i))
        0
        bits


floatFromParts :
    { mantissaBits : List ( Int, Int )
    , unbiasedExponent : Int
    }
    -> Float
floatFromParts { mantissaBits, unbiasedExponent } =
    (1 + fraction mantissaBits) * 2 ^ toFloat unbiasedExponent


bitsToList : ( Int, Int ) -> List ( Int, Int )
bitsToList ( high, low ) =
    let
        go : Int -> Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
        go howMany startAt bits acc =
            if howMany <= 0 then
                acc

            else
                go
                    (howMany - 1)
                    startAt
                    (Bitwise.shiftRightBy 1 bits)
                    (( startAt + howMany, Bitwise.and 1 bits ) :: acc)
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
        round (1 / 0)

    else
        let
            unbiased =
                e - bias
        in
        if unbiased < 0 then
            10000 - unbiased

        else
            unbiased


updateMantissa : Int -> ( Int, Int ) -> ( Int, Int )
updateMantissa unbiasedExponent ( mantissaHigh, mantissaLow ) =
    if unbiasedExponent <= 0 then
        Bitwise.reverse52Bits ( mantissaHigh, mantissaLow )

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
        ( newMantissaHigh, newMantissaLow )

    else
        ( mantissaHigh, mantissaLow )


nastyFloats : List Float
nastyFloats =
    let
        specificExamples : List Float
        specificExamples =
            {- Manually ordered as in Hypothesis: we don't have a way (that I know of
                - @janiczek) to get mantissa : Int and exponent : Int from a Float so we
               let Python do it and we just copy what we see it output.
            -}
            [ 0.0
            , 1.0e7
            , 9007199254740992
            , 1.5
            , 1.1
            , 1.9
            , 2 + 1.0e-5
            , 3.402823466e38
            , 1.7976931348623157e308
            , 0.5
            , 1 - 1.0e-5
            , 1.0 / 3
            , 1.0e-5
            , 1.192092896e-7
            , 2.220446049250313e-16
            , 1.175494351e-38
            , 2.2250738585072014e-308
            ]

        nonfiniteValues : List Float
        nonfiniteValues =
            [ 1 / 0 -- inf
            , 0 / 0 -- nan
            ]

        allPositive : List Float
        allPositive =
            specificExamples ++ List.fastConcatMap (List.repeat 5) nonfiniteValues
    in
    allPositive ++ List.map negate allPositive
