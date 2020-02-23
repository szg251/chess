module Data.Field exposing (Field, fieldSize, fileToX, parser, rankToY, serialize, view)

import Data.File as File exposing (File)
import Data.Rank as Rank exposing (Rank)
import Parser exposing ((|=), Parser, succeed)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)


parser : Parser Field
parser =
    succeed Tuple.pair
        |= File.parser
        |= Rank.parser


serialize : Field -> String
serialize ( file, rank ) =
    File.serialize file ++ Rank.serialize rank


type alias Field =
    ( File, Rank )


fieldSize : Int
fieldSize =
    45


fileToX : File -> Int
fileToX file =
    (File.toInt file - 1) * fieldSize + 1


rankToY : Rank -> Int
rankToY rank =
    (8 - Rank.toInt rank) * fieldSize + 1


getColor : List Field -> File -> Rank -> String
getColor selectedFields file rank =
    if List.any ((==) ( file, rank )) selectedFields then
        "#05a"

    else if modBy 2 (File.toInt file + 7 - Rank.toInt rank) == 0 then
        "#fff"

    else
        "#444"


view : List Field -> File -> Rank -> Svg msg
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
