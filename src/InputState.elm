module InputState exposing (..)

import Field exposing (Field)
import File exposing (File)
import List.Extra as ListE
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



-- | WithBoth File Rank


selectionHelperParser : Parser SelectionHelper
selectionHelperParser =
    oneOf
        [ Parser.map WithFile File.parser
        , Parser.map WithRank Rank.parser

        -- , Parser.map2 WithBoth File.parser Rank.parser
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


getPromotesTo : List ExtraInfo -> Maybe PieceType
getPromotesTo extraInfo =
    case
        ListE.find
            (\info ->
                case info of
                    PromotesTo _ ->
                        True

                    _ ->
                        False
            )
            extraInfo
    of
        Just (PromotesTo pieceType) ->
            Just pieceType

        _ ->
            Nothing


serialize : InputState -> String
serialize inputState =
    let
        serializeTakes extraInfo =
            if List.any ((==) Takes) extraInfo then
                "x"

            else
                ""

        serializeEnPassant extraInfo =
            if List.any ((==) EnPassant) extraInfo then
                "e.p."

            else
                ""

        serializePromotesTo extraInfo =
            case getPromotesTo extraInfo of
                Just pieceType ->
                    "=" ++ Piece.serialize pieceType

                _ ->
                    ""
    in
    case inputState of
        NotSelected ->
            ""

        Selected pieceType selectionHelper ->
            Piece.serialize pieceType
                ++ serializeSelectionHelper selectionHelper

        Moved pieceType selectionHelper field extraInfo ->
            Piece.serialize pieceType
                ++ serializeSelectionHelper selectionHelper
                ++ serializeTakes extraInfo
                ++ Field.serialize field
                ++ serializePromotesTo extraInfo
                ++ serializeEnPassant extraInfo

        Castled QueenSide ->
            "0-0-0"

        Castled KingSide ->
            "0-0"
