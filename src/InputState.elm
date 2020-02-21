module InputState exposing (..)

import Field exposing (Field)
import File exposing (File)
import Maybe.Extra as MaybeE
import Parser exposing ((|.), (|=), Parser, backtrackable, end, keyword, oneOf, succeed, symbol)
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank exposing (Rank)


type InputState
    = NotSelected
    | Selected PieceType SelectionHelper
    | Moved PieceType SelectionHelper Field (List ExtraInfo)
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


type ExtraInfo
    = Takes
    | PromoteTo Piece


parser : Parser InputState
parser =
    let
        toMoved piece selectionHelper maybeTakes field =
            let
                extraInfo =
                    MaybeE.toList maybeTakes
            in
            Moved piece selectionHelper field extraInfo
    in
    oneOf
        [ succeed NotSelected
            |. end
        , backtrackable <|
            succeed toMoved
                |= Piece.parser
                |= succeed NoSelectionHelper
                |= oneOf
                    [ succeed (Just Takes)
                        |. symbol "x"
                    , succeed Nothing
                    ]
                |= Field.parser
                |. end
        , backtrackable <|
            succeed toMoved
                |= Piece.parser
                |= backtrackable selectionHelperParser
                |= oneOf
                    [ succeed (Just Takes)
                        |. symbol "x"
                    , succeed Nothing
                    ]
                |= Field.parser
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

        Moved pieceType selectionHelper field _ ->
            Piece.serialize pieceType
                ++ serializeSelectionHelper selectionHelper
                ++ Field.serialize field

        Castled QueenSide ->
            "0-0-0"

        Castled KingSide ->
            "0-0"
