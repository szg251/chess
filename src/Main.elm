module Main exposing (main)

import Board
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, div, text)
import Json.Decode as D
import Maybe.Extra as MaybeE
import Piece exposing (Color(..), Piece(..))


type alias Model =
    { selected : Maybe Piece
    , rotate : Int
    , inputBuffer : Maybe Char
    , pieces : List Piece
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Nothing
      , rotate = 0
      , inputBuffer = Nothing
      , pieces = Board.initPieces
      }
    , Cmd.none
    )


type Msg
    = Input String


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
        Input value ->
            case model.inputBuffer of
                Nothing ->
                    ( { model | inputBuffer = stringToFile value }, Cmd.none )

                Just bufferedFile ->
                    case stringToRank value of
                        Nothing ->
                            ( model, Cmd.none )

                        Just rank ->
                            let
                                newSelection =
                                    ( bufferedFile, rank )
                            in
                            case model.selected of
                                Just currentSelection ->
                                    let
                                        movedPiece =
                                            Piece.move model.pieces newSelection currentSelection

                                        pieces =
                                            movedPiece
                                                :: List.filter (\piece -> Piece.getField piece /= Piece.getField movedPiece) model.pieces
                                    in
                                    ( { model
                                        | pieces = pieces
                                        , selected = Nothing
                                        , inputBuffer = Nothing
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( { model
                                        | selected =
                                            model.pieces
                                                |> List.filter (\piece -> Piece.getField piece == newSelection)
                                                |> List.head
                                        , pieces =
                                            model.pieces
                                                |> List.filter (\piece -> Piece.getField piece /= newSelection)
                                        , inputBuffer = Nothing
                                      }
                                    , Cmd.none
                                    )


view : Model -> Html Msg
view model =
    div []
        [ Board.view 0
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
        |> D.map Input


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyPress keyDecoder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
