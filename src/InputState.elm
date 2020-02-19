module InputState exposing (..)

import Field exposing (Field)
import File exposing (File)
import Parser exposing ((|.), (|=), Parser, backtrackable, end, keyword, oneOf, succeed, symbol)
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank exposing (Rank)


type InputState
    = NotSelected
    | Selected PieceType SelectionHelper
    | Moved PieceType SelectionHelper Field
    | Castled Side


type Side
    = KingSide
    | QueenSide


type SelectionHelper
    = NoSelectionHelper
    | WithFile File
    | WithRank Rank


selectionHelperParser : Parser SelectionHelper
selectionHelperParser =
    oneOf
        [ Parser.map WithFile File.parser
        , Parser.map WithRank Rank.parser
        , succeed NoSelectionHelper
        ]


serializeSelectionHelper : SelectionHelper -> String
serializeSelectionHelper selectionHelper =
    case selectionHelper of
        NoSelectionHelper ->
            ""

        WithFile file ->
            File.serialize file

        WithRank rank ->
            Rank.serialize rank


parser : Parser InputState
parser =
    oneOf
        [ succeed NotSelected
            |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= succeed NoSelectionHelper
                |. oneOf [ succeed () |. symbol "x", succeed () ]
                |= Field.parser
                |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= selectionHelperParser
                |= Field.parser
                |. oneOf [ succeed () |. symbol "x", succeed () ]
                |. end
        , backtrackable <|
            succeed Selected
                |= Piece.parser
                |= succeed NoSelectionHelper
                |. end
        , succeed Selected
            |= Piece.parser
            |= selectionHelperParser
            |. end
        , succeed (Castled QueenSide)
            |. keyword "0-0-0"
            |. end
        , succeed (Castled KingSide)
            |. keyword "0-0"
            |. end
        ]


serialize : InputState -> String
serialize inputState =
    case inputState of
        NotSelected ->
            ""

        Selected pieceType selectionHelper ->
            Piece.serialize pieceType

        Moved pieceType selectionHelper field ->
            Piece.serialize pieceType
                ++ serializeSelectionHelper selectionHelper
                ++ Field.serialize field

        Castled QueenSide ->
            "0-0-0"

        Castled KingSide ->
            "0-0"
