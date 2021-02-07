module Decimal exposing (..)

import List.Extra exposing (dropWhileRight)
import Parser exposing (..)


type alias Decimal =
    { mantissa : Int, exponent : Int }


decimal : Int -> Int -> Decimal
decimal mantissa exponent =
    { mantissa = mantissa, exponent = exponent }


normalize : Decimal -> Decimal
normalize { mantissa, exponent } =
    if modBy 10 mantissa == 0 then
        normalize { mantissa = mantissa // 10, exponent = exponent + 1 }

    else
        { mantissa = mantissa, exponent = exponent }


widen : Int -> Decimal -> Decimal
widen e a =
    { mantissa = a.mantissa * (10 ^ (a.exponent - e)), exponent = e }


normalizeMin : Decimal -> Decimal -> ( Int, Decimal, Decimal )
normalizeMin a b =
    let
        minExp =
            min a.exponent b.exponent
    in
    ( minExp, widen minExp a, widen minExp b )



-- 12.12 + 1.123
-- repr: 1212 -2 and 1123 -3
-- result: 13.243
-- result repr: 13243


add : Decimal -> Decimal -> Decimal
add a1 a2 =
    let
        ( minExp, b1, b2 ) =
            normalizeMin a1 a2
    in
    { mantissa = b1.mantissa + b2.mantissa, exponent = minExp }


sub : Decimal -> Decimal -> Decimal
sub a1 a2 =
    let
        ( minExp, b1, b2 ) =
            normalizeMin a1 a2
    in
    { mantissa = b1.mantissa - b2.mantissa, exponent = minExp }



-- 100 * 0.1 = 10
-- (1, 2) * (1, -1) = (1, 1)
-- 20 * 0.5 =
-- (2, 1) * (5, -1) = (1, 1)


mul : Decimal -> Decimal -> Decimal
mul a1 a2 =
    normalize { mantissa = a1.mantissa * a2.mantissa, exponent = a1.exponent + a2.exponent }


cmpHelp : (Int -> Int -> a) -> Decimal -> Decimal -> a
cmpHelp op a1 a2 =
    let
        ( _, b1, b2 ) =
            normalizeMin a1 a2
    in
    op b1.mantissa b2.mantissa


le : Decimal -> Decimal -> Bool
le a b =
    cmpHelp (<=) a b


ge : Decimal -> Decimal -> Bool
ge a b =
    cmpHelp (>=) a b


lt : Decimal -> Decimal -> Bool
lt a b =
    cmpHelp (<) a b


gt : Decimal -> Decimal -> Bool
gt a b =
    cmpHelp (>) a b


eq : Decimal -> Decimal -> Bool
eq a b =
    cmpHelp (==) a b


neq : Decimal -> Decimal -> Bool
neq a b =
    cmpHelp (/=) a b


digits : Parser ()
digits =
    chompIf Char.isDigit |. chompWhile Char.isDigit


optDigits : Parser ()
optDigits =
    chompWhile Char.isDigit


optMinus : Parser ()
optMinus =
    oneOf [ symbol "-", succeed () ]


expSign : Parser ()
expSign =
    oneOf [ symbol "-", symbol "+", succeed () ]


natPart : Parser ()
natPart =
    oneOf
        [ symbol "0"
        , chompIf (\c -> c /= '0' && Char.isDigit c) |. optDigits
        ]


integerPart : Parser String
integerPart =
    getChompedString <| optMinus |. natPart


fractionalPart : Parser String
fractionalPart =
    oneOf
        [ getChompedString <| symbol "." |. digits
        , succeed ".0"
        ]


exponentIndicator : Parser ()
exponentIndicator =
    chompIf (\c -> c == 'e' || c == 'E')


exponentPart : Parser String
exponentPart =
    oneOf
        [ getChompedString <| exponentIndicator |. expSign |. digits
        , succeed "e0"
        ]


doStuff : String -> String -> String -> Decimal
doStuff int frac exp =
    let
        -- drop the comma and trailing zeros
        frac_ =
            String.dropLeft 1 frac |> String.toList |> dropWhileRight ((==) '0') |> String.fromList

        -- parse drop + end 'e's, parse and return as int
        -- e-10 becomes -10
        exp_ =
            String.replace "+" "" exp |> String.dropLeft 1 |> String.toInt |> Maybe.withDefault 0

        -- Mantissa for 123.123e12 is 123123. Exponent is 9
        mantissa =
            (int ++ frac_) |> String.toInt |> Maybe.withDefault 0
    in
    { mantissa = mantissa, exponent = exp_ - String.length frac_ }


decimalParser : Parser Decimal
decimalParser =
    succeed doStuff |= integerPart |= fractionalPart |= exponentPart |. end


fromString : String -> Maybe Decimal
fromString s =
    Parser.run decimalParser s |> Result.toMaybe


toString : Decimal -> String
toString d =
    String.fromInt d.mantissa ++ "E" ++ String.fromInt d.exponent


fromInt : Int -> Decimal
fromInt n =
    normalize { mantissa = n, exponent = 0 }



-- May involve loss of precision


toFloat : Decimal -> Float
toFloat d =
    toString d |> String.toFloat |> Maybe.withDefault (0.0 / 0.0)
