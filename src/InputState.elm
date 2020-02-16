module InputState exposing (..)

import Field exposing (Field)
import File exposing (File)
import Parser exposing ((|.), (|=), Parser, backtrackable, end, oneOf, succeed)
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank exposing (Rank)


type InputState
    = NotSelected
    | Selected PieceType (Maybe SelectionHelper)
    | Moved PieceType (Maybe SelectionHelper) Field


type SelectionHelper
    = WithFile File
    | WithRank Rank


selectionHelperParser : Parser SelectionHelper
selectionHelperParser =
    oneOf
        [ Parser.map WithFile File.parser
        , Parser.map WithRank Rank.parser
        ]


parser : Parser InputState
parser =
    oneOf
        [ succeed NotSelected
            |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= succeed Nothing
                |= Field.parser
                |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= Parser.map Just selectionHelperParser
                |= Field.parser
                |. end
        , backtrackable <|
            succeed Selected
                |= Piece.parser
                |= succeed Nothing
                |. end
        , succeed Selected
            |= Piece.parser
            |= Parser.map Just selectionHelperParser
            |. end
        ]


getSelectedPieces : InputState -> Color -> List Piece -> List Piece
getSelectedPieces inputState turn pieces =
    let
        selectByNameAndFile name file =
            List.filter
                (\piece ->
                    (piece.name == name)
                        && (piece.color == turn)
                        && (Tuple.first piece.field == file)
                )
                pieces

        selectByNameAndRank name rank =
            List.filter
                (\piece ->
                    (piece.name == name)
                        && (piece.color == turn)
                        && (Tuple.second piece.field == rank)
                )
                pieces

        selectByName name =
            List.filter (\piece -> piece.name == name && piece.color == turn) pieces
    in
    case inputState of
        Selected name (Just (WithFile file)) ->
            selectByNameAndFile name file

        Moved name (Just (WithFile file)) _ ->
            selectByNameAndFile name file

        Selected name (Just (WithRank rank)) ->
            selectByNameAndRank name rank

        Moved name (Just (WithRank rank)) _ ->
            selectByNameAndRank name rank

        Selected name Nothing ->
            selectByName name

        Moved name Nothing _ ->
            selectByName name

        _ ->
            []
