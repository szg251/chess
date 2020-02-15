module Board exposing (initPieces, view)

import Field
import File
import Piece exposing (Color(..), Piece(..))
import Rank
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (height, style, viewBox, width)


cartesianProduct : List a -> List b -> (a -> b -> c) -> List c
cartesianProduct xs ys combine =
    List.concatMap (\x -> List.map (\y -> combine x y) ys) xs


initPieces : List Piece
initPieces =
    [ Pawn Black ( File.a, Rank.r7 )
    , Pawn Black ( File.b, Rank.r7 )
    , Pawn Black ( File.c, Rank.r7 )
    , Pawn Black ( File.d, Rank.r7 )
    , Pawn Black ( File.e, Rank.r7 )
    , Pawn Black ( File.f, Rank.r7 )
    , Pawn Black ( File.g, Rank.r7 )
    , Pawn Black ( File.h, Rank.r7 )
    , Rook Black ( File.a, Rank.r8 )
    , Knight Black ( File.b, Rank.r8 )
    , Bishop Black ( File.c, Rank.r8 )
    , Queen Black ( File.d, Rank.r8 )
    , King Black ( File.e, Rank.r8 )
    , Bishop Black ( File.f, Rank.r8 )
    , Knight Black ( File.g, Rank.r8 )
    , Rook Black ( File.h, Rank.r8 )
    , Pawn White ( File.a, Rank.r2 )
    , Pawn White ( File.b, Rank.r2 )
    , Pawn White ( File.c, Rank.r2 )
    , Pawn White ( File.d, Rank.r2 )
    , Pawn White ( File.e, Rank.r2 )
    , Pawn White ( File.f, Rank.r2 )
    , Pawn White ( File.g, Rank.r2 )
    , Pawn White ( File.h, Rank.r2 )
    , Rook White ( File.a, Rank.r1 )
    , Knight White ( File.b, Rank.r1 )
    , Bishop White ( File.c, Rank.r1 )
    , Queen White ( File.d, Rank.r1 )
    , King White ( File.e, Rank.r1 )
    , Bishop White ( File.f, Rank.r1 )
    , Knight White ( File.g, Rank.r1 )
    , Rook White ( File.h, Rank.r1 )
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
                [ Rank.r1, Rank.r2, Rank.r3, Rank.r4, Rank.r5, Rank.r6, Rank.r7, Rank.r8 ]
                (Field.view (Maybe.map Piece.getField selected))
                ++ List.map (Piece.view rotation) pieces
            )
        ]
