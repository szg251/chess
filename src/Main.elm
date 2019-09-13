module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, div, text)
import Json.Decode as D
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, transform, viewBox, width, x, y)


type alias Model =
    { selected : Maybe Field
    , rotate : Int
    , inputBuffer : Maybe Char
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = Nothing
      , rotate = 0
      , inputBuffer = Nothing
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
                                    Just ( bufferedFile, rank )
                            in
                            ( { model
                                | selected =
                                    if newSelection == model.selected then
                                        Nothing

                                    else
                                        newSelection
                                , inputBuffer = Nothing
                              }
                            , Cmd.none
                            )


type Color
    = White
    | Black
    | Selected


type alias Field =
    ( Char, Int )


fileToInt : Char -> Int
fileToInt file =
    case file of
        'a' ->
            1

        'b' ->
            2

        'c' ->
            3

        'd' ->
            4

        'e' ->
            5

        'f' ->
            6

        'g' ->
            7

        'h' ->
            8

        _ ->
            0


viewField : Maybe Field -> Char -> Int -> Svg Msg
viewField selected file rank =
    rect
        [ x <| String.fromInt ((fileToInt file - 1) * 100 + 1)
        , y <| String.fromInt ((8 - rank) * 100 + 1)
        , width "100"
        , height "100"
        , fill <|
            case getColor selected file rank of
                White ->
                    "#fff"

                Black ->
                    "#000"

                Selected ->
                    "#05a"
        , stroke "#000"
        , strokeWidth "1"
        ]
        []


cartesianProduct : List a -> List b -> (a -> b -> c) -> List c
cartesianProduct xs ys combine =
    List.concatMap (\x -> List.map (\y -> combine x y) ys) xs


getColor : Maybe Field -> Char -> Int -> Color
getColor selected file rank =
    if selected == Just ( file, rank ) then
        Selected

    else if modBy 2 (fileToInt file + 7 - rank) == 0 then
        White

    else
        Black


view : Model -> Html Msg
view model =
    div []
        [ svg [ viewBox "0 0 802 802" ]
            [ g
                [ transform <| "rotate(" ++ String.fromInt model.rotate ++ " 401 401)" ]
                (cartesianProduct
                    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' ]
                    (List.range 1 8)
                    (viewField model.selected)
                )
            ]
        , div []
            [ text <|
                case model.inputBuffer of
                    Nothing ->
                        case model.selected of
                            Nothing ->
                                ""

                            Just ( file, rank ) ->
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
