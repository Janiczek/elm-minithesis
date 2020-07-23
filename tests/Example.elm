module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Minithesis as M
import Minithesis.Fuzz as MF
import Test


listOfIntegers : MF.Fuzzer (List Int)
listOfIntegers =
    MF.list (MF.int 0 10000)


findsSmallList : M.Test (List Int)
findsSmallList =
    M.test "My awesome test" listOfIntegers <|
        \fuzzedList ->
            List.sum fuzzedList <= 1000


suite : Test.Test
suite =
    Test.fuzz Fuzz.int "Finds an counterexample and successfully shrinks it" <|
        \seed ->
            M.run seed findsSmallList
                |> Expect.equal ( "My awesome test", M.FailsWith [ 1001 ] )
