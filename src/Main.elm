module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Data.Board as Board
import Data.Piece exposing (Color(..), Piece, PieceType(..))
import File exposing (File)
import File.Download as Download
import File.Select as Select
import GameLogic exposing (GameState)
import History
import Html exposing (Html, a, br, button, div, form, input, label, strong, text)
import Html.Attributes exposing (checked, disabled, for, href, id, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InputState exposing (InputState(..))
import Parser
import Result.Extra as ResultE
import Task


type alias Model =
    { gameState : GameState
    , rotateOnTurn : Bool
    , input : String
    , inputState : InputState
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = GameLogic.init
      , rotateOnTurn = True
      , input = ""
      , inputState = NotSelected
      , error = Nothing
      }
    , Task.attempt (\_ -> NoOp) (Dom.focus "input-bar")
    )


type Msg
    = NoOp
    | Input String
    | Move
    | RotateOnTurnClicked
    | Restart
    | LoadButtonClicked
    | SaveButtonClicked
    | FileSelected File
    | FileLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RotateOnTurnClicked ->
            ( { model | rotateOnTurn = not model.rotateOnTurn }, Cmd.none )

        Restart ->
            ( { gameState = GameLogic.init
              , rotateOnTurn = True
              , input = ""
              , inputState = NotSelected
              , error = Nothing
              }
            , Cmd.none
            )

        Input str ->
            let
                inputState =
                    str
                        |> Parser.run InputState.parser
                        |> Result.withDefault NotSelected
            in
            ( { model
                | input = str
                , inputState = inputState
              }
            , Cmd.none
            )

        Move ->
            case GameLogic.getNextGameState model.inputState model.gameState of
                Ok gameState ->
                    ( { model
                        | input = ""
                        , inputState = NotSelected
                        , gameState = gameState
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | error = Just err
                        , input = ""
                        , inputState = NotSelected
                      }
                    , Cmd.none
                    )

        SaveButtonClicked ->
            ( model, Download.string "chess.txt" "text/plain" (History.serialize model.gameState.history) )

        LoadButtonClicked ->
            ( model, Select.file [ "text/plain" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded file ->
            case Parser.run History.parser file |> Debug.log "parsed" of
                Ok history ->
                    case GameLogic.loadHistory history of
                        Ok gameState ->
                            ( { model
                                | input = ""
                                , inputState = NotSelected
                                , gameState = gameState
                                , error = Nothing
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model
                                | error = Just err
                                , input = ""
                                , inputState = NotSelected
                              }
                            , Cmd.none
                            )

                Err err ->
                    ( { model | error = Just "Couldn't parse file" }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( selectedFields, error ) =
            GameLogic.getSelectedFields model.inputState model.gameState
                |> (\result -> ( Result.withDefault [] result, ResultE.error result ))
    in
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
            (if model.rotateOnTurn && model.gameState.turn == Black then
                180

             else
                0
            )
            selectedFields
            model.gameState.pieces
        , div []
            [ text "The pieces are controlled by "
            , a
                [ href "https://www.cheatography.com/davechild/cheat-sheets/chess-algebraic-notation/"
                , target "blank"
                ]
                [ text "algebraic notation" ]
            , text " like Nc3"
            ]
        , form [ onSubmit Move ]
            [ input [ id "input-bar", onInput Input, value model.input ] []
            ]
        , strong [] [ text <| Maybe.withDefault "" error ]
        , button [ onClick SaveButtonClicked ] [ text "Save to file" ]
        , button [ onClick LoadButtonClicked ] [ text "Load from file" ]
        , History.view model.gameState.history
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
