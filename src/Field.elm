module Field exposing (Field, fieldSize, fileToX, rankToY, view)

import File exposing (File)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)


type Rank
    = Rank Int


type alias Field =
    ( File, Int )


fieldSize : Int
fieldSize =
    45


fileToX : File -> Int
fileToX file =
    (File.toInt file - 1) * fieldSize + 1


rankToY : Int -> Int
rankToY rank =
    (8 - rank) * fieldSize + 1


getColor : Maybe Field -> File -> Int -> String
getColor selected file rank =
    if selected == Just ( file, rank ) then
        "#05a"

    else if modBy 2 (File.toInt file + 7 - rank) == 0 then
        "#fff"

    else
        "#444"


view : Maybe Field -> File -> Int -> Svg msg
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
