module Field exposing (Field, fieldSize, fileToInt, fileToX, intToFile, rankToY, view)

import Svg exposing (Svg, rect)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)


type alias Field =
    ( Char, Int )


fieldSize : Int
fieldSize =
    45


fileToInt : Char -> Int
fileToInt file =
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


intToFile : Int -> Char
intToFile file =
    case file of
        1 ->
            'a'

        2 ->
            'b'

        3 ->
            'c'

        4 ->
            'd'

        5 ->
            'e'

        6 ->
            'f'

        7 ->
            'g'

        8 ->
            'h'

        _ ->
            ' '


fileToX : Char -> Int
fileToX file =
    (fileToInt file - 1) * fieldSize + 1


rankToY : Int -> Int
rankToY rank =
    (8 - rank) * fieldSize + 1


getColor : Maybe Field -> Char -> Int -> String
getColor selected file rank =
    if selected == Just ( file, rank ) then
        "#05a"

    else if modBy 2 (fileToInt file + 7 - rank) == 0 then
        "#fff"

    else
        "#444"


view : Maybe Field -> Char -> Int -> Svg msg
view selected file rank =
    rect
        [ x <| String.fromInt (fileToX file)
        , y <| String.fromInt (rankToY rank)
        , width <| String.fromInt fieldSize
        , height <| String.fromInt fieldSize
        , fill <| getColor selected file rank
        , stroke "#444"
        , strokeWidth "1"
        ]
        []
