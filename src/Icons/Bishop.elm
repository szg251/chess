module Icons.Bishop exposing (black, white)

import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, style, transform, version, x, y)


white : Int -> Int -> Int -> Svg msg
white rotation posX posY =
    let
        rotate =
            "rotate("
                ++ String.join " "
                    [ String.fromInt rotation
                    , String.fromInt (22 + posX)
                    , String.fromInt (22 + posY)
                    ]
                ++ ")"
    in
    svg
        [ version "1.1"
        , x <| String.fromInt posX
        , y <| String.fromInt posY
        , transform rotate
        ]
        [ g [ style "opacity:1; fill:none; fill-rule:evenodd; fill-opacity:1; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#ffffff; stroke:#000000; stroke-linecap:butt;" ] [ Svg.path [ d "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.646,38.99 6.677,38.97 6,38 C 7.354,36.06 9,36 9,36 z" ] [], Svg.path [ d "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z" ] [], Svg.path [ d "M 25 8 A 2.5 2.5 0 1 1  20,8 A 2.5 2.5 0 1 1  25 8 z" ] [] ], Svg.path [ d "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [] ] ]


black : Int -> Int -> Int -> Svg msg
black rotation posX posY =
    let
        rotate =
            "rotate("
                ++ String.join " "
                    [ String.fromInt rotation
                    , String.fromInt (22 + posX)
                    , String.fromInt (22 + posY)
                    ]
                ++ ")"
    in
    svg
        [ version "1.1"
        , x <| String.fromInt posX
        , y <| String.fromInt posY
        , transform rotate
        ]
        [ g [ style "opacity:1; fill:none; fill-rule:evenodd; fill-opacity:1; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#000000; stroke:#000000; stroke-linecap:butt;" ] [ Svg.path [ d "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.646,38.99 6.677,38.97 6,38 C 7.354,36.06 9,36 9,36 z" ] [], Svg.path [ d "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z" ] [], Svg.path [ d "M 25 8 A 2.5 2.5 0 1 1  20,8 A 2.5 2.5 0 1 1  25 8 z" ] [] ], Svg.path [ d "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18", style "fill:none; stroke:#ffffff; stroke-linejoin:miter;" ] [] ] ]
