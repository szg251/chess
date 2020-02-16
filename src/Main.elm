module Main exposing (main)

import Board
import Browser
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
    = Input String
    | RotateOnTurnClicked
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            let
                inputState =
                    str
                        |> Parser.run InputState.parser
                        |> Result.withDefault NotSelected
            in
            case inputState of
                Moved _ _ _ ->
                    ( { model
                        | input = ""
                        , inputState = NotSelected
                        , pieces = GameLogic.movePiece inputState model.turn model.pieces
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
