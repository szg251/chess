module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events
import Data.Field exposing (Field)
import Data.Piece exposing (Color(..), Piece, PieceType(..))
import File exposing (File)
import File.Download as Download
import File.Select as Select
import GameLogic exposing (GameState)
import History
import Html exposing (Attribute, Html, a, br, button, div, form, input, label, strong, text)
import Html.Attributes exposing (checked, disabled, for, href, id, name, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InputState exposing (InputState(..))
import Parser
import Result.Extra as ResultE
import Task
import View.Board as Board


type alias Model =
    { gameState : GameState
    , replayedState : Maybe ( Int, List Piece )
    , viewpoint : Viewpoint
    , isTouchMode : Bool
    , input : String
    , inputState : InputState
    , selectedFields : List Field
    , error : Maybe String
    , boardSize : Int
    , isMobile : Bool
    }


type Viewpoint
    = BlackSide
    | WhiteSide
    | Rotating


viewpointToString : Viewpoint -> String
viewpointToString viewpoint =
    case viewpoint of
        BlackSide ->
            "BlackSide"

        WhiteSide ->
            "WhiteSide"

        Rotating ->
            "Rotating"


viewpointFromString : String -> Maybe Viewpoint
viewpointFromString viewpoint =
    case viewpoint of
        "BlackSide" ->
            Just BlackSide

        "WhiteSide" ->
            Just WhiteSide

        "Rotating" ->
            Just Rotating

        _ ->
            Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = GameLogic.init
      , replayedState = Nothing
      , viewpoint = WhiteSide
      , isTouchMode = False
      , input = ""
      , inputState = NotSelected
      , selectedFields = []
      , error = Nothing
      , boardSize = 700
      , isMobile = False
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
    | ChangeViewpoint String
    | ShowTouchKeyboardClicked
    | Restart
    | Replayed Int
    | LoadButtonClicked
    | SaveButtonClicked
    | FileSelected File
    | FileLoaded String



-- | TimeTravel (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resized width height ->
            let
                isMobile =
                    width < 900
            in
            ( { model
                | boardSize =
                    if isMobile then
                        min width height

                    else
                        600
                , isMobile = isMobile
              }
            , Cmd.none
            )

        GotViewport { viewport } ->
            let
                isMobile =
                    viewport.width < 900
            in
            ( { model
                | boardSize =
                    if isMobile then
                        min viewport.width viewport.height |> round

                    else
                        600
                , isMobile = isMobile
              }
            , Cmd.none
            )

        ChangeViewpoint value ->
            case viewpointFromString value of
                Nothing ->
                    ( model, Cmd.none )

                Just viewpoint ->
                    ( { model | viewpoint = viewpoint }, Cmd.none )

        ShowTouchKeyboardClicked ->
            ( { model | isTouchMode = not model.isTouchMode }, Cmd.none )

        Restart ->
            ( { model
                | gameState = GameLogic.init
                , input = ""
                , inputState = NotSelected
                , selectedFields = []
                , error = Nothing
              }
            , Cmd.none
            )

        Replayed stepNum ->
            let
                history =
                    model.gameState.history
            in
            ( { model
                | replayedState =
                    if stepNum == List.length history - 1 then
                        Nothing

                    else
                        history
                            |> List.drop (List.length history - (stepNum + 1))
                            |> GameLogic.loadHistory
                            |> Result.toMaybe
                            |> Maybe.map .pieces
                            |> Maybe.map (Tuple.pair stepNum)
              }
            , Cmd.none
            )

        Input str ->
            let
                inputState =
                    str
                        |> Parser.run InputState.parser
                        |> Result.withDefault NotSelected

                ( selectedFields, error ) =
                    GameLogic.getSelectedFields inputState model.gameState
                        |> (\result -> ( Result.withDefault [] result, ResultE.error result ))
            in
            ( { model
                | input = str
                , inputState = inputState
                , selectedFields = selectedFields
                , error = error
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
                    ( { model | error = Just err }, Cmd.none )

        SaveButtonClicked ->
            ( model, Download.string "chess.txt" "text/plain" (History.serialize model.gameState.history) )

        LoadButtonClicked ->
            ( model, Select.file [ "text/plain" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded file ->
            case Parser.run History.parser file of
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
    div []
        [ div [ style "display" "flex", style "flex-wrap" "wrap" ]
            [ Board.view
                { rotation =
                    if
                        (model.viewpoint == Rotating && model.gameState.turn == Black)
                            || (model.viewpoint == BlackSide)
                    then
                        180

                    else
                        0
                , boardSize = model.boardSize
                , selected = model.selectedFields
                , pieces =
                    model.replayedState
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault model.gameState.pieces
                , isTouchMode = model.isTouchMode
                , input = model.input
                , error = model.error
                }
            , div
                [ if model.isMobile then
                    style "width" "100%"

                  else
                    style "width" "calc(100% - 600px)"
                ]
                [ if model.isTouchMode then
                    div []
                        [ viewTouchKeyboard model.input
                        , strong [] [ text <| Maybe.withDefault "" model.error ]
                        ]

                  else
                    form [ onSubmit Move ]
                        [ input [ id "input-bar", onInput Input, value model.input ] []
                        , strong [] [ text <| Maybe.withDefault "" model.error ]
                        ]
                , div []
                    [ text "The pieces are controlled by "
                    , a
                        [ href "https://www.cheatography.com/davechild/cheat-sheets/chess-algebraic-notation/"
                        , target "blank"
                        ]
                        [ text "algebraic notation" ]
                    , text " like Nc3"
                    ]
                , input
                    [ id "viewpoint-white"
                    , name "viewpoint"
                    , type_ "radio"
                    , value (viewpointToString WhiteSide)
                    , checked (model.viewpoint == WhiteSide)
                    , onInput ChangeViewpoint
                    ]
                    []
                , label [ for "viewpoint-white" ] [ text "Play as white" ]
                , input
                    [ id "viewpoint-black"
                    , name "viewpoint"
                    , type_ "radio"
                    , checked (model.viewpoint == BlackSide)
                    , value (viewpointToString BlackSide)
                    , onInput ChangeViewpoint
                    ]
                    []
                , label [ for "viewpoint-black" ] [ text "Play as black" ]
                , input
                    [ id "viewpoint-rotating"
                    , name "viewpoint"
                    , type_ "radio"
                    , checked (model.viewpoint == Rotating)
                    , value (viewpointToString Rotating)
                    , onInput ChangeViewpoint
                    ]
                    []
                , label [ for "viewpoint-rotating" ] [ text "Rotating" ]
                , input
                    [ id "show-touch-keyboard"
                    , type_ "checkbox"
                    , checked model.isTouchMode
                    , onClick ShowTouchKeyboardClicked
                    ]
                    []
                , label [ for "show-touch-keyboard" ] [ text "Display touch keyboard" ]
                , div []
                    [ button [ onClick Restart ] [ text "Restart" ]
                    , button [ onClick SaveButtonClicked ] [ text "Save to file" ]
                    , button [ onClick LoadButtonClicked ] [ text "Load from file" ]
                    ]
                , History.view model.gameState.history
                    (Maybe.map Tuple.first model.replayedState)
                    Replayed
                ]
            ]
        ]


viewTouchKey : List (Attribute Msg) -> String -> Msg -> Html Msg
viewTouchKey attrs label msg =
    let
        width =
            if String.length label > 2 then
                "25%"

            else
                "12.5%"
    in
    button
        ([ style "width" width
         , style "height" "50px"
         , style "font-size" "25px"
         , style "text-align" "center"
         , onClick msg
         ]
            ++ attrs
        )
        [ text label ]


viewTouchKeyboard : String -> Html Msg
viewTouchKeyboard prevString =
    let
        duplicate fn val =
            fn val val

        viewCharKey key =
            viewTouchKey [] key (Input (prevString ++ key))
    in
    div [ style "width" "100%" ]
        [ div [] (List.map viewCharKey [ "R", "N", "B", "Q", "K" ])
        , div [] (List.map viewCharKey [ "a", "b", "c", "d", "e", "f", "g", "h" ])
        , div [] (List.map viewCharKey [ "1", "2", "3", "4", "5", "6", "7", "8" ])
        , div [] (List.map viewCharKey [ "x", "0-0", "0-0-0", "=", "e.p." ])
        , viewTouchKey [] "⌫" (Input (String.dropRight 1 prevString))
        , viewTouchKey [ style "margin-left" "75%" ] "⏎ " Move
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
