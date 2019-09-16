module Main exposing (main)

import Board
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, for, id, type_, value)
import Html.Events exposing (onClick)
import Json.Decode as D
import Maybe.Extra as MaybeE
import Piece exposing (Color(..), Piece(..))


type alias Model =
    { selected : Maybe Piece
    , inputBuffer : Maybe Char
    , pieces : List Piece
    , turn : Color
    , rotateOnTurn : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Nothing
      , inputBuffer = Nothing
      , pieces = Board.initPieces
      , turn = White
      , rotateOnTurn = True
      }
    , Cmd.none
    )


type Msg
    = InputFile Char
    | InputRank Int
    | Cancel
    | RotateOnTurnClicked


isValidFile : Char -> Bool
isValidFile char =
    let
        code =
            Char.toCode char
    in
    0x61 <= code && code <= 0x68


isValidRank : Int -> Bool
isValidRank int =
    1 <= int && int <= 8


stringToFile : String -> Maybe Char
stringToFile str =
    String.uncons str
        |> Maybe.map Tuple.first
        |> Maybe.andThen
            (\char ->
                if isValidFile char then
                    Just char

                else
                    Nothing
            )


stringToRank : String -> Maybe Int
stringToRank str =
    String.toInt str
        |> Maybe.andThen
            (\int ->
                if isValidRank int then
                    Just int

                else
                    Nothing
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Cancel ->
            ( { model
                | inputBuffer = Nothing
                , selected = Nothing
                , pieces = MaybeE.toList model.selected ++ model.pieces
              }
            , Cmd.none
            )

        InputFile file ->
            ( { model | inputBuffer = Just file }, Cmd.none )

        InputRank rank ->
            case model.inputBuffer of
                Nothing ->
                    ( model, Cmd.none )

                Just bufferedFile ->
                    let
                        newSelection =
                            ( bufferedFile, rank )
                    in
                    case model.selected of
                        Nothing ->
                            let
                                nextSelected =
                                    model.pieces
                                        |> List.filter
                                            (\piece ->
                                                (Piece.getField piece == newSelection)
                                                    && (Piece.getColor piece == model.turn)
                                            )
                                        |> List.head
                            in
                            ( { model
                                | selected = nextSelected
                                , pieces =
                                    model.pieces
                                        |> List.filter (\piece -> Just piece /= nextSelected)
                                , inputBuffer = Nothing
                              }
                            , Cmd.none
                            )

                        Just currentSelection ->
                            case Piece.move model.pieces newSelection currentSelection of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just movedPiece ->
                                    ( { model
                                        | pieces =
                                            movedPiece
                                                :: List.filter (\piece -> Piece.getField piece /= Piece.getField movedPiece) model.pieces
                                        , selected = Nothing
                                        , inputBuffer = Nothing
                                        , turn =
                                            if model.turn == White then
                                                Black

                                            else
                                                White
                                      }
                                    , Cmd.none
                                    )

        RotateOnTurnClicked ->
            ( { model | rotateOnTurn = not model.rotateOnTurn }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input
                [ id "is-rotating"
                , type_ "checkbox"
                , checked model.rotateOnTurn
                , onClick RotateOnTurnClicked
                ]
                []
            , label [ for "is-rotating" ] [ text "Rotate on turns" ]
            ]
        , Board.view
            (if model.rotateOnTurn && model.turn == Black then
                180

             else
                0
            )
            model.selected
            (MaybeE.toList model.selected ++ model.pieces)
        , div []
            [ text <|
                case model.inputBuffer of
                    Nothing ->
                        case model.selected of
                            Nothing ->
                                ""

                            Just selectedPiece ->
                                let
                                    ( file, rank ) =
                                        Piece.getField selectedPiece
                                in
                                String.fromChar file ++ String.fromInt rank

                    Just bufferedFile ->
                        String.fromChar bufferedFile
            ]
        ]


keyDecoder : D.Decoder Msg
keyDecoder =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Escape" then
                    D.succeed Cancel

                else
                    case
                        MaybeE.or
                            (stringToFile key |> Maybe.map InputFile)
                            (stringToRank key |> Maybe.map InputRank)
                    of
                        Just msg ->
                            D.succeed msg

                        Nothing ->
                            D.fail ("invalid input: " ++ key)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
