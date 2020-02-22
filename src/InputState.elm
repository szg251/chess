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
    | PromotesTo PieceType
    | EnPassant


parser : Parser InputState
parser =
    let
        toMoved piece ( selectionHelper, maybeTakes, field ) maybePromotion maybeEnPassant =
            let
                extraInfo =
                    MaybeE.values [ maybeTakes, maybePromotion, maybeEnPassant ]
            in
            Moved piece selectionHelper field extraInfo

        promotionParser =
            oneOf
                [ succeed (Just << PromotesTo)
                    |. symbol "="
                    |= Piece.parser
                , succeed Nothing
                ]

        takesParser =
            oneOf
                [ succeed (Just Takes)
                    |. symbol "x"
                , succeed Nothing
                ]

        enPassantParser =
            oneOf
                [ succeed (Just EnPassant)
                    |. keyword "e.p."
                , succeed Nothing
                ]

        selectionHelperToTargetParser =
            oneOf
                [ succeed (\x y z -> ( x, y, z ))
                    |= succeed NoSelectionHelper
                    |= takesParser
                    |= backtrackable Field.parser
                , succeed (\x y z -> ( x, y, z ))
                    |= selectionHelperParser
                    |= takesParser
                    |= Field.parser
                ]
    in
    oneOf
        [ succeed NotSelected
            |. end
        , backtrackable <|
            succeed toMoved
                |= Piece.parser
                |= selectionHelperToTargetParser
                |= promotionParser
                |= enPassantParser
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
