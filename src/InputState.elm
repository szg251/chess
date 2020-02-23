module InputState exposing (..)

import Data.Field as Field exposing (Field)
import Data.File as File exposing (File)
import Data.Piece as Piece exposing (Color(..), Piece, PieceType(..))
import Data.Rank as Rank exposing (Rank)
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Parser exposing ((|.), (|=), Parser, backtrackable, end, keyword, oneOf, succeed, symbol)


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
    | WithField Field


selectionHelperParser : Parser SelectionHelper
selectionHelperParser =
    oneOf
        [ Parser.map WithFile <| backtrackable File.parser
        , Parser.map WithRank <| backtrackable Rank.parser
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

        WithField field ->
            Field.serialize field


type ExtraInfo
    = Takes
    | PromotesTo PieceType
    | EnPassant
    | Check
    | Checkmate


parser : Parser InputState
parser =
    let
        toMoved piece ( selectionHelper, maybeTakes, field ) maybePromotion maybeEnPassant maybeCheck =
            let
                extraInfo =
                    MaybeE.values [ maybeTakes, maybePromotion, maybeEnPassant, maybeCheck ]
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

        checkParser =
            oneOf
                [ succeed (Just Check)
                    |. symbol "+"
                , succeed (Just Checkmate)
                    |. symbol "#"
                , succeed Nothing
                ]

        selectionHelperToTargetParser =
            oneOf
                [ succeed (\x y z -> ( x, y, z ))
                    |= Parser.map WithField (backtrackable Field.parser)
                    |= takesParser
                    |= Field.parser
                , succeed (\x y z -> ( x, y, z ))
                    |= succeed NoSelectionHelper
                    |= backtrackable takesParser
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
                |= checkParser
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


takes : List ExtraInfo -> Bool
takes =
    List.any ((==) Takes)


enPassant : List ExtraInfo -> Bool
enPassant =
    List.any ((==) EnPassant)


check : List ExtraInfo -> Bool
check =
    List.any ((==) Check)


checkmate : List ExtraInfo -> Bool
checkmate =
    List.any ((==) Checkmate)


serialize : InputState -> String
serialize inputState =
    let
        serializeTakes extraInfo =
            if takes extraInfo then
                "x"

            else
                ""

        serializeEnPassant extraInfo =
            if enPassant extraInfo then
                "e.p."

            else
                ""

        serializeCheck extraInfo =
            if check extraInfo then
                "+"

            else if checkmate extraInfo then
                "#"

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
                ++ serializeCheck extraInfo

        Castled QueenSide ->
            "0-0-0"

        Castled KingSide ->
            "0-0"
