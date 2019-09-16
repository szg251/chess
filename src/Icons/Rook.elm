module Icons.Rook exposing (black, white)

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
        [ g [ style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 34,14 L 31,17 L 14,17 L 11,14" ] [], Svg.path [ d "M 31,17 L 31,29.5 L 14,29.5 L 14,17", style "stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 31,29.5 L 32.5,32 L 12.5,32 L 14,29.5" ] [], Svg.path [ d "M 11,14 L 34,14", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [] ] ]


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
        [ g [ style "opacity:1; fill:000000; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12.5,32 L 14,29.5 L 31,29.5 L 32.5,32 L 12.5,32 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 14,29.5 L 14,16.5 L 31,16.5 L 31,29.5 L 14,29.5 z ", style "stroke-linecap:butt;stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,16.5 L 11,14 L 34,14 L 31,16.5 L 14,16.5 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14 L 11,14 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,35.5 L 33,35.5 L 33,35.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 13,31.5 L 32,31.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,29.5 L 31,29.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,16.5 L 31,16.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 11,14 L 34,14", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [] ] ]
