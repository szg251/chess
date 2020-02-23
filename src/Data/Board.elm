module Data.Board exposing (initPieces, view)

import Data.Field as Field exposing (Field)
import Data.File as File
import Data.Piece as Piece exposing (Color(..), Piece, PieceType(..))
import Data.Rank as Rank
import Svg exposing (Svg, g, rect, svg, text, text_)
import Svg.Attributes exposing (dominantBaseline, fill, height, opacity, style, textAnchor, viewBox, width, x, y)


cartesianProduct : List a -> List b -> (a -> b -> c) -> List c
cartesianProduct xs ys combine =
    List.concatMap (\x -> List.map (\y -> combine x y) ys) xs


initPieces : List Piece
initPieces =
    [ Piece Pawn Black ( File.a, Rank.r7 )
    , Piece Pawn Black ( File.b, Rank.r7 )
    , Piece Pawn Black ( File.c, Rank.r7 )
    , Piece Pawn Black ( File.d, Rank.r7 )
    , Piece Pawn Black ( File.e, Rank.r7 )
    , Piece Pawn Black ( File.f, Rank.r7 )
    , Piece Pawn Black ( File.g, Rank.r7 )
    , Piece Pawn Black ( File.h, Rank.r7 )
    , Piece Rook Black ( File.a, Rank.r8 )
    , Piece Knight Black ( File.b, Rank.r8 )
    , Piece Bishop Black ( File.c, Rank.r8 )
    , Piece Queen Black ( File.d, Rank.r8 )
    , Piece King Black ( File.e, Rank.r8 )
    , Piece Bishop Black ( File.f, Rank.r8 )
    , Piece Knight Black ( File.g, Rank.r8 )
    , Piece Rook Black ( File.h, Rank.r8 )
    , Piece Pawn White ( File.a, Rank.r2 )
    , Piece Pawn White ( File.b, Rank.r2 )
    , Piece Pawn White ( File.c, Rank.r2 )
    , Piece Pawn White ( File.d, Rank.r2 )
    , Piece Pawn White ( File.e, Rank.r2 )
    , Piece Pawn White ( File.f, Rank.r2 )
    , Piece Pawn White ( File.g, Rank.r2 )
    , Piece Pawn White ( File.h, Rank.r2 )
    , Piece Rook White ( File.a, Rank.r1 )
    , Piece Knight White ( File.b, Rank.r1 )
    , Piece Bishop White ( File.c, Rank.r1 )
    , Piece Queen White ( File.d, Rank.r1 )
    , Piece King White ( File.e, Rank.r1 )
    , Piece Bishop White ( File.f, Rank.r1 )
    , Piece Knight White ( File.g, Rank.r1 )
    , Piece Rook White ( File.h, Rank.r1 )
    ]


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
                                    [ rect [ y "90%", width "100%", height "10%", opacity "0.8", fill "white" ] []
                                    , text_
                                        [ x "50%"
                                        , y "95%"
                                        , dominantBaseline "middle"
                                        , textAnchor "middle"
                                        , style "font: bold 13px sans-serif; fill: red"
                                        ]
                                        [ text <| Maybe.withDefault "" error ]
                                    ]
                           )

                else
                    []
               )
        )
