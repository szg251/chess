module Data.File exposing (File, a, b, c, d, e, f, fromChar, fromInt, g, h, parser, serialize, toChar, toInt)

import Parser exposing ((|.), Parser, oneOf, succeed, symbol)


type File
    = File Char


parser : Parser File
parser =
    oneOf
        [ succeed (File 'a') |. symbol "a"
        , succeed (File 'b') |. symbol "b"
        , succeed (File 'c') |. symbol "c"
        , succeed (File 'd') |. symbol "d"
        , succeed (File 'e') |. symbol "e"
        , succeed (File 'f') |. symbol "f"
        , succeed (File 'g') |. symbol "g"
        , succeed (File 'h') |. symbol "h"
        ]


serialize : File -> String
serialize (File char) =
    String.fromChar char


toInt : File -> Int
toInt (File file) =
    case file of
        'a' ->
            1

        'b' ->
            2

        'c' ->
            3

        'd' ->
            4

        'e' ->
            5

        'f' ->
            6

        'g' ->
            7

        'h' ->
            8

        _ ->
            0


toChar : File -> Char
toChar (File file) =
    file


fromInt : Int -> Maybe File
fromInt file =
    case file of
        1 ->
            Just (File 'a')

        2 ->
            Just (File 'b')

        3 ->
            Just (File 'c')

        4 ->
            Just (File 'd')

        5 ->
            Just (File 'e')

        6 ->
            Just (File 'f')

        7 ->
            Just (File 'g')

        8 ->
            Just (File 'h')

        _ ->
            Nothing


fromChar : Char -> Maybe File
fromChar char =
    let
        code =
            Char.toCode char
    in
    if 0x61 <= code && code <= 0x68 then
        Just (File char)

    else
        Nothing


a : File
a =
    File 'a'


b : File
b =
    File 'b'


c : File
c =
    File 'c'


d : File
d =
    File 'd'


e : File
e =
    File 'e'


f : File
f =
    File 'f'


g : File
g =
    File 'g'


h : File
h =
    File 'h'
