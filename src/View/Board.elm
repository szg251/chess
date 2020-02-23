module View.Board exposing (view)

import Data.Field as Field exposing (Field)
import Data.File as File
import Data.Piece as Piece exposing (Color(..), Piece, PieceType(..))
import Data.Rank as Rank
import Svg exposing (Svg, g, rect, svg, text, text_)
import Svg.Attributes exposing (dominantBaseline, fill, height, opacity, style, textAnchor, viewBox, width, x, y)


cartesianProduct : List a -> List b -> (a -> b -> c) -> List c
cartesianProduct xs ys combine =
    List.concatMap (\x -> List.map (\y -> combine x y) ys) xs


type alias Props =
    { rotation : Int
    , boardSize : Int
    , selected : List Field
    , pieces : List Piece
    , isTouchMode : Bool
    , input : String
    , error : Maybe String
    }


view : Props -> Svg msg
view { rotation, boardSize, selected, pieces, isTouchMode, input, error } =
    let
        size =
            Field.fieldSize * 8 + 2

        viewBoxSize =
            String.join " " [ "0 0", String.fromInt size, String.fromInt size ]
    in
    svg
        [ viewBox viewBoxSize
        , width <| String.fromInt boardSize
        , height <| String.fromInt boardSize
        ]
        ([ g
            [ style <|
                "transition: all 1s ease-out;transform-origin:center;transform: rotate("
                    ++ String.fromInt rotation
                    ++ "deg);"
            ]
            (cartesianProduct
                [ File.a, File.b, File.c, File.d, File.e, File.f, File.g, File.h ]
                [ Rank.r1, Rank.r2, Rank.r3, Rank.r4, Rank.r5, Rank.r6, Rank.r7, Rank.r8 ]
                (Field.view selected)
                ++ List.map (Piece.view rotation) pieces
            )
         ]
            ++ (if isTouchMode then
                    [ text_
                        [ x "50%"
                        , y "50%"
                        , dominantBaseline "middle"
                        , textAnchor "middle"
                        , style "font: bold 80px sans-serif; fill: #f7b360"
                        ]
                        [ text input ]
                    ]
                        ++ (case error of
                                Nothing ->
                                    []

                                Just errText ->
                                    let
                                        wrapped =
                                            wrapLine errText

                                        boxSize =
                                            (List.length wrapped + 1) * 15
                                    in
                                    [ rect
                                        [ y <| String.fromInt (size - boxSize)
                                        , width "100%"
                                        , height <| String.fromInt boxSize
                                        , opacity "0.8"
                                        , fill "white"
                                        ]
                                        []
                                    ]
                                        ++ List.indexedMap
                                            (\index message ->
                                                text_
                                                    [ x "50%"
                                                    , y <| String.fromInt (size - 15 - (index * 15))
                                                    , dominantBaseline "middle"
                                                    , textAnchor "middle"
                                                    , style "font: bold 13px sans-serif; fill: red"
                                                    ]
                                                    [ text message ]
                                            )
                                            wrapped
                           )

                else
                    []
               )
        )


wrapLine : String -> List String
wrapLine message =
    String.split " " message
        |> List.foldl
            (\word acc ->
                case acc of
                    ( length, str ) :: rest ->
                        let
                            newLength =
                                length + String.length word + 1
                        in
                        if newLength < 50 then
                            ( newLength, str ++ " " ++ word ) :: rest

                        else
                            ( String.length word, word ) :: ( length, str ) :: rest

                    [] ->
                        [ ( String.length word, word ) ]
            )
            []
        |> List.map Tuple.second
