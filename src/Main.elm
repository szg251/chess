module Main exposing (main)

import Board
import Browser
import Browser.Dom as Dom
import File exposing (File)
import GameLogic
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (checked, for, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import InputState exposing (InputState(..))
import Maybe.Extra as MaybeE
import Parser
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank
import Task


type alias Model =
    { selected : Maybe Piece
    , pieces : List Piece
    , turn : Color
    , rotateOnTurn : Bool
    , input : String
    , inputState : InputState
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Nothing
      , pieces = Board.initPieces
      , turn = White
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
    | RotateOnTurnClicked
    | Restart


changeTurn : Color -> Color
changeTurn color =
    case color of
        White ->
            Black

        Black ->
            White


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RotateOnTurnClicked ->
            ( { model | rotateOnTurn = not model.rotateOnTurn }, Cmd.none )

        Restart ->
            ( { selected = Nothing
              , pieces = Board.initPieces
              , turn = White
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
            case inputState of
                Moved _ _ _ ->
                    case GameLogic.movePiece inputState model.turn model.pieces of
                        Ok nextPieces ->
                            ( { model
                                | input = ""
                                , inputState = NotSelected
                                , pieces = nextPieces
                                , turn = changeTurn model.turn
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

                _ ->
                    ( { model
                        | input = str
                        , inputState = inputState
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
            (InputState.getSelectedPieces model.inputState model.turn model.pieces)
            (MaybeE.toList model.selected ++ model.pieces)
        , div [] [ text "The pieces are controlled by algebraic chess notation like Nc3" ]
        , input [ id "input-bar", onInput Input, value model.input ] []
        , div [ style "fontWeight" "bold" ] [ text <| Maybe.withDefault "" model.error ]
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
