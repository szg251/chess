module Board exposing (initPieces, view)

import Field
import File
import Piece exposing (Color(..), Piece(..))
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (height, style, viewBox, width)


cartesianProduct : List a -> List b -> (a -> b -> c) -> List c
cartesianProduct xs ys combine =
    List.concatMap (\x -> List.map (\y -> combine x y) ys) xs


initPieces : List Piece
initPieces =
    [ Pawn Black ( File.a, 7 )
    , Pawn Black ( File.b, 7 )
    , Pawn Black ( File.c, 7 )
    , Pawn Black ( File.d, 7 )
    , Pawn Black ( File.e, 7 )
    , Pawn Black ( File.f, 7 )
    , Pawn Black ( File.g, 7 )
    , Pawn Black ( File.h, 7 )
    , Rook Black ( File.a, 8 )
    , Knight Black ( File.b, 8 )
    , Bishop Black ( File.c, 8 )
    , Queen Black ( File.d, 8 )
    , King Black ( File.e, 8 )
    , Bishop Black ( File.f, 8 )
    , Knight Black ( File.g, 8 )
    , Rook Black ( File.h, 8 )
    , Pawn White ( File.a, 2 )
    , Pawn White ( File.b, 2 )
    , Pawn White ( File.c, 2 )
    , Pawn White ( File.d, 2 )
    , Pawn White ( File.e, 2 )
    , Pawn White ( File.f, 2 )
    , Pawn White ( File.g, 2 )
    , Pawn White ( File.h, 2 )
    , Rook White ( File.a, 1 )
    , Knight White ( File.b, 1 )
    , Bishop White ( File.c, 1 )
    , Queen White ( File.d, 1 )
    , King White ( File.e, 1 )
    , Bishop White ( File.f, 1 )
    , Knight White ( File.g, 1 )
    , Rook White ( File.h, 1 )
    ]


view : Int -> Maybe Piece -> List Piece -> Svg msg
view rotation selected pieces =
    let
        size =
            Field.fieldSize * 8 + 2

        boardSize =
            String.join " " [ "0 0", String.fromInt size, String.fromInt size ]
    in
    svg
        [ viewBox boardSize
        , width "500"
        , height "500"
        ]
        [ g
            [ style <|
                "transition: all 1s ease-out;transform-origin:center;transform: rotate("
                    ++ String.fromInt rotation
                    ++ "deg);"
            ]
            (cartesianProduct
                [ File.a, File.b, File.c, File.d, File.e, File.f, File.g, File.h ]
                (List.range 1 8)
                (Field.view (Maybe.map Piece.getField selected))
                ++ List.map (Piece.view rotation) pieces
            )
        ]
