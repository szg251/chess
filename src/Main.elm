module Main exposing (main)

import Board
import Browser
import Browser.Dom as Dom
import GameLogic exposing (GameState)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (checked, for, id, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InputState exposing (InputState(..))
import Parser
import Piece exposing (Color(..), Piece, PieceType(..))
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
            case GameLogic.movePiece model.inputState model.gameState of
                Ok ( _, _, gameState ) ->
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
        , div [] [ text "The pieces are controlled by algebraic chess notation like Nc3" ]
        , form [ onSubmit Move ]
            [ input [ id "input-bar", onInput Input, value model.input ] []
            ]
        , div [ style "fontWeight" "bold" ] [ text <| Maybe.withDefault "" error ]
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
