module File exposing (File, a, b, c, d, e, f, fromChar, fromInt, g, h, toChar, toInt)


type File
    = File Char


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


fromInt : Int -> File
fromInt file =
    case file of
        1 ->
            File 'a'

        2 ->
            File 'b'

        3 ->
            File 'c'

        4 ->
            File 'd'

        5 ->
            File 'e'

        6 ->
            File 'f'

        7 ->
            File 'g'

        8 ->
            File 'h'

        _ ->
            File ' '


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
