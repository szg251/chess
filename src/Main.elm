module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events
import Data.Board as Board
import Data.Piece exposing (Color(..), Piece, PieceType(..))
import File exposing (File)
import File.Download as Download
import File.Select as Select
import GameLogic exposing (GameState)
import History
import Html exposing (Html, a, br, button, div, form, input, label, strong, text)
import Html.Attributes exposing (checked, disabled, for, href, id, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InputState exposing (InputState(..))
import Parser
import Result.Extra as ResultE
import Task


type alias Model =
    { gameState : GameState
    , rotateOnTurn : Bool
    , showTouchKeyboard : Bool
    , input : String
    , inputState : InputState
    , error : Maybe String
    , boardSize : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = GameLogic.init
      , rotateOnTurn = True
      , showTouchKeyboard = False
      , input = ""
      , inputState = NotSelected
      , error = Nothing
      , boardSize = 700
      }
    , Cmd.batch
        [ Task.attempt (\_ -> NoOp) (Dom.focus "input-bar")
        , Task.perform GotViewport Dom.getViewport
        ]
    )


type Msg
    = NoOp
    | Resized Int Int
    | GotViewport Viewport
    | Input String
    | Move
    | RotateOnTurnClicked
    | ShowTouchKeyboardClicked
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

        Resized width height ->
            ( { model | boardSize = min width height }, Cmd.none )

        GotViewport { viewport } ->
            ( { model | boardSize = min viewport.width viewport.height |> round }, Cmd.none )

        RotateOnTurnClicked ->
            ( { model | rotateOnTurn = not model.rotateOnTurn }, Cmd.none )

        ShowTouchKeyboardClicked ->
            ( { model | showTouchKeyboard = not model.showTouchKeyboard }, Cmd.none )

        Restart ->
            ( { model
                | gameState = GameLogic.init
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
        [ div [ style "display" "flex", style "flex-wrap" "wrap" ]
            [ Board.view
                (if model.rotateOnTurn && model.gameState.turn == Black then
                    180

                 else
                    0
                )
                model.boardSize
                selectedFields
                model.gameState.pieces
            , div []
                [ div []
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
                    , strong [] [ text <| Maybe.withDefault "" error ]
                    ]
                , input
                    [ id "is-rotating"
                    , type_ "checkbox"
                    , checked model.rotateOnTurn
                    , onClick RotateOnTurnClicked
                    ]
                    []
                , label [ for "is-rotating" ] [ text "Rotate on turns" ]
                , input
                    [ id "show-touch-keyboard"
                    , type_ "checkbox"
                    , checked model.showTouchKeyboard
                    , onClick ShowTouchKeyboardClicked
                    ]
                    []
                , label [ for "show-touch-keyboard" ] [ text "Display touch keyboard" ]
                , div []
                    [ button [ onClick Restart ] [ text "Restart" ]
                    , button [ onClick SaveButtonClicked ] [ text "Save to file" ]
                    , button [ onClick LoadButtonClicked ] [ text "Load from file" ]
                    ]
                , if model.showTouchKeyboard then
                    viewTouchKeyboard model.input

                  else
                    text ""
                , History.view model.gameState.history
                ]
            ]
        ]


viewTouchKey : String -> String -> Html Msg
viewTouchKey prevString buttonValue =
    button
        [ style "width" "100px"
        , style "height" "100px"
        , style "font-size" "25px"
        , onClick (Input (prevString ++ buttonValue))
        ]
        [ text buttonValue ]


viewTouchKeyboard : String -> Html Msg
viewTouchKeyboard prevString =
    div []
        [ div [] (List.map (viewTouchKey prevString) [ "R", "N", "B", "Q", "K" ])
        , div [] (List.map (viewTouchKey prevString) [ "a", "b", "c", "d", "e", "f", "g", "h" ])
        , div [] (List.map (viewTouchKey prevString) [ "1", "2", "3", "4", "5", "6", "7", "8" ])
        , div [] (List.map (viewTouchKey prevString) [ "x", "0-0", "0-0-0", "e.p." ])
        , button
            [ style "width" "100px"
            , style "height" "100px"
            , style "font-size" "25px"
            , onClick Move
            ]
            [ text "enter" ]
        , button
            [ style "width" "100px"
            , style "height" "100px"
            , style "font-size" "25px"
            , onClick (Input "")
            ]
            [ text "clear" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resized


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
