module Board exposing (initPieces, view)

import Field
import Piece exposing (Color(..), Piece(..))
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (height, transform, viewBox, width)


cartesianProduct : List a -> List b -> (a -> b -> c) -> List c
cartesianProduct xs ys combine =
    List.concatMap (\x -> List.map (\y -> combine x y) ys) xs


initPieces : List Piece
initPieces =
    [ Pawn Black ( 'a', 7 )
    , Pawn Black ( 'b', 7 )
    , Pawn Black ( 'c', 7 )
    , Pawn Black ( 'd', 7 )
    , Pawn Black ( 'e', 7 )
    , Pawn Black ( 'f', 7 )
    , Pawn Black ( 'g', 7 )
    , Pawn Black ( 'h', 7 )
    , Rook Black ( 'a', 8 )
    , Knight Black ( 'b', 8 )
    , Bishop Black ( 'c', 8 )
    , Queen Black ( 'd', 8 )
    , King Black ( 'e', 8 )
    , Bishop Black ( 'f', 8 )
    , Knight Black ( 'g', 8 )
    , Rook Black ( 'h', 8 )
    , Pawn White ( 'a', 2 )
    , Pawn White ( 'b', 2 )
    , Pawn White ( 'c', 2 )
    , Pawn White ( 'd', 2 )
    , Pawn White ( 'e', 2 )
    , Pawn White ( 'f', 2 )
    , Pawn White ( 'g', 2 )
    , Pawn White ( 'h', 2 )
    , Rook White ( 'a', 1 )
    , Knight White ( 'b', 1 )
    , Bishop White ( 'c', 1 )
    , Queen White ( 'd', 1 )
    , King White ( 'e', 1 )
    , Bishop White ( 'f', 1 )
    , Knight White ( 'g', 1 )
    , Rook White ( 'h', 1 )
    ]


view : Int -> Maybe Piece -> List Piece -> Svg msg
view rotation selected pieces =
    let
        size =
            String.fromInt (Field.fieldSize * 8 + 2)

        boardSize =
            String.join " " [ "0 0", size, size ]
    in
    svg
        [ viewBox boardSize
        , width "500"
        , height "500"
        ]
        [ g
            [ transform <| "rotate(" ++ String.fromInt rotation ++ " 401 401)" ]
            (cartesianProduct
                [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' ]
                (List.range 1 8)
                (Field.view (Maybe.map Piece.getField selected))
                ++ List.map Piece.view pieces
            )
        ]
