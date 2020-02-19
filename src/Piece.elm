module Piece exposing (Color(..), Field, Piece, PieceType(..), parser, serialize, view)

import Field exposing (fileToX, rankToY)
import File exposing (File)
import Icons.Bishop
import Icons.King
import Icons.Knight
import Icons.Pawn
import Icons.Queen
import Icons.Rook
import Parser exposing ((|.), Parser, oneOf, succeed, symbol)
import Rank exposing (Rank)
import Svg exposing (Svg)


parser : Parser PieceType
parser =
    oneOf
        [ succeed Rook |. symbol "R"
        , succeed Knight |. symbol "N"
        , succeed Bishop |. symbol "B"
        , succeed Queen |. symbol "Q"
        , succeed King |. symbol "K"
        , succeed Pawn |. symbol ""
        ]


serialize : PieceType -> String
serialize pieceType =
    case pieceType of
        Pawn ->
            ""

        Rook ->
            "R"

        Knight ->
            "N"

        Bishop ->
            "B"

        Queen ->
            "Q"

        King ->
            "K"


type alias Field =
    ( File, Rank )


type PieceType
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King


type alias Piece =
    { name : PieceType
    , color : Color
    , field : Field
    }


type Color
    = White
    | Black


view : Int -> Piece -> Svg msg
view rotate piece =
    let
        ( file, rank ) =
            piece.field
    in
    case ( piece.name, piece.color ) of
        ( Pawn, White ) ->
            Icons.Pawn.white rotate (fileToX file) (rankToY rank)

        ( Pawn, Black ) ->
            Icons.Pawn.black rotate (fileToX file) (rankToY rank)

        ( King, White ) ->
            Icons.King.white rotate (fileToX file) (rankToY rank)

        ( King, Black ) ->
            Icons.King.black rotate (fileToX file) (rankToY rank)

        ( Queen, White ) ->
            Icons.Queen.white rotate (fileToX file) (rankToY rank)

        ( Queen, Black ) ->
            Icons.Queen.black rotate (fileToX file) (rankToY rank)

        ( Rook, White ) ->
            Icons.Rook.white rotate (fileToX file) (rankToY rank)

        ( Rook, Black ) ->
            Icons.Rook.black rotate (fileToX file) (rankToY rank)

        ( Knight, White ) ->
            Icons.Knight.white rotate (fileToX file) (rankToY rank)

        ( Knight, Black ) ->
            Icons.Knight.black rotate (fileToX file) (rankToY rank)

        ( Bishop, White ) ->
            Icons.Bishop.white rotate (fileToX file) (rankToY rank)

        ( Bishop, Black ) ->
            Icons.Bishop.black rotate (fileToX file) (rankToY rank)
