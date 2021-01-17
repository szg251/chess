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
import History exposing (History)
import Html exposing (Attribute, Html, a, br, button, div, form, input, label, strong, text)
import Html.Attributes exposing (checked, disabled, for, href, id, name, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InputState exposing (InputState(..))
import Json.Decode as Decode
import Maybe.Extra as MaybeE
import Parser
import Result.Extra as ResultE
import Task
import View.Board as Board


type alias Model =
    { gameState : GameState
    , replayedState : Maybe ( Int, GameState )
    , alternateHistory : Maybe String
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


getActiveGameState : Model -> GameState
getActiveGameState model =
    Maybe.withDefault model.gameState (Maybe.map Tuple.second model.replayedState)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = GameLogic.init
      , replayedState = Nothing
      , alternateHistory = Nothing
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
    | ToggleEdit
    | HistoryEdited String
    | PrevStep
    | NextStep
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

        ToggleEdit ->
            ( case model.alternateHistory of
                Nothing ->
                    { model | alternateHistory = Just (History.serialize model.gameState.history) }

                Just raw ->
                    case Parser.run History.parser raw of
                        Ok history ->
                            case GameLogic.loadHistory history of
                                Ok gameState ->
                                    { model
                                        | input = ""
                                        , inputState = NotSelected
                                        , gameState = gameState
                                        , error = Nothing
                                        , alternateHistory = Nothing
                                    }

                                Err err ->
                                    { model
                                        | error = Just err
                                        , input = ""
                                        , inputState = NotSelected
                                        , alternateHistory = Nothing
                                    }

                        Err err ->
                            { model
                                | error =
                                    Just
                                        ("Couldn't parse history: " ++ deadEndsToString err)
                            }
            , Cmd.none
            )

        Restart ->
            ( { model
                | gameState = GameLogic.init
                , input = ""
                , inputState = NotSelected
                , selectedFields = []
                , error = Nothing
                , replayedState = Nothing
              }
            , Cmd.none
            )

        Replayed stepNum ->
            ( { model
                | replayedState = replayHistory model.gameState.history stepNum
                , selectedFields = []
              }
            , Cmd.none
            )

        HistoryEdited history ->
            ( { model | alternateHistory = Just history }
            , Cmd.none
            )

        PrevStep ->
            let
                lastStepNum =
                    model.replayedState
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault (List.length model.gameState.history - 1)
            in
            ( { model
                | replayedState =
                    replayHistory
                        model.gameState.history
                        (max (lastStepNum - 1) 0)
                , selectedFields = []
              }
            , Cmd.none
            )

        NextStep ->
            let
                historyLength =
                    List.length model.gameState.history

                lastStepNum =
                    model.replayedState
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault historyLength
            in
            ( { model
                | replayedState =
                    replayHistory
                        model.gameState.history
                        (min (lastStepNum + 1) historyLength)
                , selectedFields = []
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
                    GameLogic.getSelectedFields inputState (getActiveGameState model)
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
            case GameLogic.getNextGameState model.inputState (getActiveGameState model) of
                Ok gameState ->
                    ( { model
                        | input = ""
                        , inputState = NotSelected
                        , gameState = gameState
                        , error = Nothing
                        , replayedState = Nothing
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
            ( loadHistory file model, Cmd.none )


loadHistory : String -> Model -> Model
loadHistory raw model =
    case Parser.run History.parser raw of
        Ok history ->
            case GameLogic.loadHistory history of
                Ok gameState ->
                    { model
                        | input = ""
                        , inputState = NotSelected
                        , gameState = gameState
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | error = Just err
                        , input = ""
                        , inputState = NotSelected
                    }

        Err err ->
            { model | error = Just ("Couldn't parse history: " ++ deadEndsToString err) }


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString =
    let
        deadEndToString { row, col } =
            "Problem at row " ++ String.fromInt row ++ " col " ++ String.fromInt col
    in
    List.map deadEndToString
        >> String.join "\n"


replayHistory : History -> Int -> Maybe ( Int, GameState )
replayHistory history stepNum =
    if stepNum == List.length history - 1 then
        Nothing

    else
        history
            |> List.drop (List.length history - (stepNum + 1))
            |> GameLogic.loadHistory
            |> Result.toMaybe
            |> Maybe.map (Tuple.pair stepNum)


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
                    (getActiveGameState model).pieces
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
                    [ text "This chess is intentionally \"hard to use\", the goal is to get familiar with "
                    , a
                        [ href "https://www.cheatography.com/davechild/cheat-sheets/chess-algebraic-notation/"
                        , target "blank"
                        ]
                        [ text "algebraic notation" ]
                    , text ". Therefore, you can only move the pieces on the board using keyboard commands like Nc3."
                    ]
                , div [ style "font-weight" "bold" ] [ text "Viewpoint" ]
                , input
                    [ id "viewpoint-white"
                    , name "viewpoint"
                    , type_ "radio"
                    , value (viewpointToString WhiteSide)
                    , checked (model.viewpoint == WhiteSide)
                    , onInput ChangeViewpoint
                    ]
                    []
                , label [ for "viewpoint-white" ] [ text "White" ]
                , input
                    [ id "viewpoint-black"
                    , name "viewpoint"
                    , type_ "radio"
                    , checked (model.viewpoint == BlackSide)
                    , value (viewpointToString BlackSide)
                    , onInput ChangeViewpoint
                    ]
                    []
                , label [ for "viewpoint-black" ] [ text "Black" ]
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
                , br [] []
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
                    , button [ onClick ToggleEdit ]
                        [ text
                            (if MaybeE.isJust model.alternateHistory then
                                "Save history"

                             else
                                "Edit history"
                            )
                        ]
                    ]
                , History.view
                    { history = model.gameState.history
                    , selected = Maybe.map Tuple.first model.replayedState
                    , alternateHistory = model.alternateHistory
                    , replayedMsg = Replayed
                    , editedMsg = HistoryEdited
                    }
                , div []
                    [ button [ onClick PrevStep ] [ text "<" ]
                    , button [ onClick NextStep ] [ text ">" ]
                    ]
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
    Sub.batch
        [ Browser.Events.onResize Resized
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "ArrowLeft" then
                    Decode.succeed PrevStep

                else if key == "ArrowRight" then
                    Decode.succeed NextStep

                else
                    Decode.fail "fail"
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
