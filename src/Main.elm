module Main exposing (main)

import Board
import Browser
import Browser.Events exposing (onKeyDown)
import File exposing (File)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (checked, for, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import InputState exposing (InputState(..))
import Json.Decode as D
import Maybe.Extra as MaybeE
import Parser
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank exposing (Rank)


type alias Model =
    { selected : Maybe Piece
    , inputBuffer : Maybe File
    , pieces : List Piece
    , turn : Color
    , rotateOnTurn : Bool
    , input : String
    , inputState : InputState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Nothing
      , inputBuffer = Nothing
      , pieces = Board.initPieces
      , turn = White
      , rotateOnTurn = True
      , input = ""
      , inputState = NotSelected
      }
    , Cmd.none
    )


type Msg
    = InputFile File
    | InputRank Rank
    | Input String
    | Cancel
    | RotateOnTurnClicked
    | Restart


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
                                                (piece.field == newSelection)
                                                    && (piece.color == model.turn)
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
                                                :: List.filter (\piece -> piece.field /= movedPiece.field) model.pieces
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

        Restart ->
            ( { selected = Nothing
              , inputBuffer = Nothing
              , pieces = Board.initPieces
              , turn = White
              , rotateOnTurn = True
              , input = ""
              , inputState = NotSelected
              }
            , Cmd.none
            )

        Input str ->
            ( { model
                | input = str
                , inputState =
                    str
                        |> Parser.run InputState.parser
                        |> Debug.log "input"
                        |> Result.withDefault NotSelected
              }
            , Cmd.none
            )


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
            , button [ onClick Restart ] [ text "Restart" ]
            ]
        , Board.view
            (if model.rotateOnTurn && model.turn == Black then
                180

             else
                0
            )
            model.selected
            (MaybeE.toList model.selected ++ model.pieces)
        , div [] [ text "The pieces are controlled by keyboard commands like e2e4" ]
        , input [ onInput Input, value model.input ] []
        , div [ style "fontWeight" "bold" ]
            [ text <|
                case model.inputBuffer of
                    Nothing ->
                        case model.selected of
                            Nothing ->
                                ""

                            Just selectedPiece ->
                                let
                                    ( file, rank ) =
                                        selectedPiece.field
                                in
                                (File.toChar >> String.fromChar) file ++ (Rank.toInt >> String.fromInt) rank

                    Just bufferedFile ->
                        (File.toChar >> String.fromChar) bufferedFile
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
                            (String.uncons key
                                |> Maybe.map Tuple.first
                                |> Maybe.andThen File.fromChar
                                |> Maybe.map InputFile
                            )
                            (String.toInt key
                                |> Maybe.andThen Rank.fromInt
                                |> Maybe.map InputRank
                            )
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
