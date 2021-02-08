module Example exposing (..)

import Decimal as D exposing (add, decimal, decimalParser, mul, normalize, sub, Decimal)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (run)
import Test exposing (..)


parserTest : Test
parserTest =
    describe "Test that the parser parses"
        [ test "simple case" <|
            \_ ->
                run decimalParser "3.30"
                    |> Expect.equal (Ok (decimal 33 -1))
        , test "blah" <|
            \_ ->
                run decimalParser "20.0"
                    |> Expect.equal (Ok (decimal 2 1))
        , test "negative without exponent" <|
            \_ ->
                run decimalParser "-1.0"
                    |> Expect.equal (Ok (decimal -1 0))
        , test "negative exponent" <|
            \_ ->
                run decimalParser "123.12e-10"
                    |> Expect.equal (Ok (decimal 12312 -12))
        , test "simple stuff" <|
            \_ ->
                run decimalParser "1e0"
                    |> Expect.equal (Ok (decimal 1 0))
        , test "exponent adds" <|
            \_ ->
                run decimalParser "123.123e3"
                    |> Expect.equal (Ok (decimal 123123 0))
        ]


normalizeTest : Test
normalizeTest =
    describe "Test of normalize"
        [ test "simple case" <|
            \_ ->
                normalize (decimal 100 0)
                    |> Expect.equal (decimal 1 2)
        , test "id if already normalized" <|
            \_ ->
                normalize (decimal 123 3)
                    |> Expect.equal (decimal 123 3)
        ]

minExpTest : Test
minExpTest =
    test "blah" <|
        \_ -> D.normalizeMin (decimal 0 0) (decimal 1 11)
        |> Expect.equal (0, {exponent = 0, mantissa = 0}, {exponent = 0, mantissa = 100000000000 })


addTest : Test
addTest =
    describe "Test that add works"
       [ test "1 + 1" <|
            \_ ->
                add (decimal 1 0) (decimal 1 0)
                    |> Expect.equal (decimal 2 0)
        , test "12.12 + 123.4 = 135.52" <|
            \_ ->
                add (decimal 1212 -2) (decimal 1234 -1)
                    |> Expect.equal (decimal 13552 -2)
        , test "12 + 1000 = 1012" <|
            \_ ->
                add (decimal 12 0) (decimal 1 3)
                    |> Expect.equal (decimal 1012 0)
        , test "0.01 + 1234 = 1234.01" <|
            \_ ->
                add (decimal 1 -2) (decimal 1234 0)
                    |> Expect.equal (decimal 123401 -2)
        , test "0.0 + 1234 = 1234" <|
            \_ ->
                add (decimal 0 0) (decimal 1234 0)
                    |> Expect.equal (decimal 1234 0)
        , test "12.34 + 0.0 = 12.34" <|
            \_ ->
                add (decimal 1234 -2) (decimal 0 0)
                    |> Expect.equal (decimal 1234 -2)
        , test "1E0 + 0E0" <|
            \_ ->
                add (decimal 1 0) (decimal 0 0)
                    |> Expect.equal (decimal 1 0)
        ]


subTest : Test
subTest =
    describe "Test that sub works"
        [ test "1000 - 0.01 = 999.99" <|
            \_ ->
                sub (decimal 1 3) (decimal 1 -2)
                    |> Expect.equal (decimal 99999 -2)
        ]


mulTest : Test
mulTest =
    describe "Test that mul works"
        [ test "123 * 0.01 = 1.23" <|
            \_ ->
                mul (decimal 123 0) (decimal 1 -2)
                    |> Expect.equal (decimal 123 -2)
        ]


cmpTest : Test
cmpTest =
    describe "Test of comparisons"
        [ test "1 > 0" <|
            \_ -> Expect.true "1 > 0" (D.gt (decimal 1 0) (decimal 0 0))
        , test "123 > 122.9" <|
            \_ -> Expect.true "123 > 122.9" (D.gt (decimal 123 0) (decimal 1229 -1))
        , test "-1 < 1" <|
            \_ -> Expect.true "-1 < 1" (D.lt (decimal -1 0) (decimal 1 0))
        , test "123 = 123.0" <|
            \_ -> Expect.true "123 = 123.0" (D.eq (decimal 123 0) (decimal 1230 -1))
        ]


fromString : Test
fromString =
    describe "fromString o toString = id"
        [ test "1" <|
            \_ ->
                D.toString (decimal 123 3)
                    |> D.fromString
                    |> Expect.equal (Just (decimal 123 3))
        , test "2" <|
            \_ ->
                D.toString (decimal 1 0)
                    |> D.fromString
                    |> Expect.equal (Just (decimal 1 0))
        ]

decimalFuzzer : Fuzzer Decimal
decimalFuzzer = Fuzz.map2 decimal (Fuzz.intRange -64000 64000) (Fuzz.intRange -2 7)


eqFuzzTests : Test
eqFuzzTests = fuzz decimalFuzzer "eq normalize" (\d -> Expect.true "blah" (D.eq d (D.normalize d)))

fromStringFuzzTests : Test
fromStringFuzzTests = fuzz decimalFuzzer "fromString o toString" (\d -> Expect.equal (Just d) (D.toString d |> D.fromString))

addComm : Test
addComm = fuzz2 decimalFuzzer decimalFuzzer "add is commutative" (\a b -> Expect.equal (add a b) (add b a))
--
addAssoc : Test
addAssoc = fuzz3 decimalFuzzer decimalFuzzer decimalFuzzer "add is associative" (\a b c -> Expect.equal (add (add a b) c) (add a (add b c)))
