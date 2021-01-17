module InputState exposing (..)

import Data.Field as Field exposing (Field)
import Data.File as File exposing (File)
import Data.Piece as Piece exposing (Color(..), Piece, PieceType(..))
import Data.Rank as Rank exposing (Rank)
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , backtrackable
        , chompIf
        , end
        , keyword
        , oneOf
        , spaces
        , succeed
        , symbol
        )


type InputState
    = NotSelected
    | Selected PieceType SelectionHelper
    | Moved PieceType SelectionHelper Field (List ExtraInfo)
    | Castled Side


type Side
    = KingSide
    | QueenSide


type Annotation
    = ExtremelyStrongMove
    | GreatMove
    | SpeculativeMove
    | DubiousMove
    | BadMove
    | Blunder
    | WhiteIsWinning
    | WhiteHasSignificantEdge
    | WhiteHasSmallEdge
    | Equality
    | UnclearAdvantage
    | BlackHasSmallEdge
    | BlackHasSignificantEdge
    | BlackIsWinning
    | OnlyMoveAvailable


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
    | Annotation Annotation


parser : Parser InputState
parser =
    let
        toMoved piece ( selectionHelper, maybeTakes, field ) maybePromotion maybeEnPassant ( maybeCheck, maybeAnnotation ) =
            let
                extraInfo =
                    MaybeE.values
                        [ maybeTakes
                        , maybePromotion
                        , maybeEnPassant
                        , maybeCheck
                        , maybeAnnotation
                        ]
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

        annotationParser =
            oneOf
                [ succeed (Just ExtremelyStrongMove)
                    |. symbol "!!"
                , succeed (Just SpeculativeMove)
                    |. symbol "!?"
                , succeed (Just GreatMove)
                    |. symbol "!"
                , succeed (Just Blunder)
                    |. symbol "??"
                , succeed (Just DubiousMove)
                    |. symbol "?!"
                , succeed (Just BadMove)
                    |. symbol "?"
                , succeed (Just WhiteIsWinning)
                    |. symbol "+-"
                , succeed (Just BlackIsWinning)
                    |. symbol "-+"
                , succeed (Just WhiteHasSignificantEdge)
                    |. symbol "+/-"
                , succeed (Just BlackHasSignificantEdge)
                    |. symbol "-/+"
                , succeed (Just WhiteHasSmallEdge)
                    |. symbol "+/="
                , succeed (Just BlackHasSmallEdge)
                    |. symbol "=+"
                , succeed (Just OnlyMoveAvailable)
                    |. symbol "□"
                , succeed (Just Equality)
                    |. symbol "="
                , succeed (Just UnclearAdvantage)
                    |. symbol "∞"
                , succeed Nothing
                ]
                |> Parser.map (Maybe.map Annotation)

        inputStateEnd =
            oneOf
                [ Parser.map (always True)
                    (backtrackable
                        (chompIf
                            (\char -> char /= ' ' && char /= '\n')
                        )
                    )
                , succeed False
                ]
                |> Parser.andThen checkEnding

        checkEnding isBadEnding =
            if isBadEnding then
                Parser.problem "Unexpected ending"

            else
                Parser.commit ()

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
            |. inputStateEnd
        , backtrackable <|
            succeed toMoved
                |= Piece.parser
                |= selectionHelperToTargetParser
                |= promotionParser
                |= enPassantParser
                |= oneOf
                    [ succeed Tuple.pair
                        |= backtrackable checkParser
                        |= backtrackable annotationParser
                        |. inputStateEnd
                    , succeed Tuple.pair
                        |= succeed Nothing
                        |= annotationParser
                        |. inputStateEnd
                    ]
        , backtrackable <|
            succeed Selected
                |= Piece.parser
                |= succeed NoSelectionHelper
                |. inputStateEnd
        , succeed Selected
            |= Piece.parser
            |= selectionHelperParser
            |. inputStateEnd
        , succeed (Castled QueenSide)
            |. oneOf [ keyword "0-0-0", keyword "O-O-O" ]
            |. inputStateEnd
        , succeed (Castled KingSide)
            |. oneOf [ keyword "0-0", keyword "O-O" ]
            |. inputStateEnd
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


hasInfo : ExtraInfo -> List ExtraInfo -> Bool
hasInfo extraInfo =
    List.any ((==) extraInfo)


hasAnnotation : Annotation -> List ExtraInfo -> Bool
hasAnnotation annotation =
    List.any ((==) (Annotation annotation))


serialize : InputState -> String
serialize inputState =
    let
        serializeTakes extraInfo =
            if hasInfo Takes extraInfo then
                "x"

            else
                ""

        serializeEnPassant extraInfo =
            if hasInfo EnPassant extraInfo then
                "e.p."

            else
                ""

        serializeCheck extraInfo =
            if hasInfo Check extraInfo then
                "+"

            else if hasInfo Checkmate extraInfo then
                "#"

            else
                ""

        serializeAnnotation extraInfo =
            if hasAnnotation ExtremelyStrongMove extraInfo then
                "!!"

            else if hasAnnotation GreatMove extraInfo then
                "!"

            else if hasAnnotation SpeculativeMove extraInfo then
                "!?"

            else if hasAnnotation DubiousMove extraInfo then
                "?!"

            else if hasAnnotation BadMove extraInfo then
                "?"

            else if hasAnnotation Blunder extraInfo then
                "??"

            else if hasAnnotation WhiteIsWinning extraInfo then
                "+-"

            else if hasAnnotation WhiteHasSignificantEdge extraInfo then
                "+/-"

            else if hasAnnotation WhiteHasSmallEdge extraInfo then
                "+/="

            else if hasAnnotation Equality extraInfo then
                "="

            else if hasAnnotation UnclearAdvantage extraInfo then
                "∞"

            else if hasAnnotation BlackHasSmallEdge extraInfo then
                "=+"

            else if hasAnnotation BlackHasSignificantEdge extraInfo then
                "-/+"

            else if hasAnnotation BlackIsWinning extraInfo then
                "-+"

            else if hasAnnotation OnlyMoveAvailable extraInfo then
                "□"

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
                ++ serializeAnnotation extraInfo

        Castled QueenSide ->
            "0-0-0"

        Castled KingSide ->
            "0-0"
