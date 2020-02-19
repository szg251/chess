module Rank exposing (Rank, fromInt, parser, r1, r2, r3, r4, r5, r6, r7, r8, serialize, toInt)

import Parser exposing ((|.), Parser, oneOf, succeed, symbol)


type Rank
    = Rank Int


parser : Parser Rank
parser =
    oneOf
        [ succeed (Rank 1) |. symbol "1"
        , succeed (Rank 2) |. symbol "2"
        , succeed (Rank 3) |. symbol "3"
        , succeed (Rank 4) |. symbol "4"
        , succeed (Rank 5) |. symbol "5"
        , succeed (Rank 6) |. symbol "6"
        , succeed (Rank 7) |. symbol "7"
        , succeed (Rank 8) |. symbol "8"
        ]


serialize : Rank -> String
serialize (Rank int) =
    String.fromInt int


fromInt : Int -> Maybe Rank
fromInt int =
    if 1 <= int && int <= 8 then
        Just (Rank int)

    else
        Nothing


toInt : Rank -> Int
toInt (Rank rank) =
    rank


r1 : Rank
r1 =
    Rank 1


r2 : Rank
r2 =
    Rank 2


r3 : Rank
r3 =
    Rank 3


r4 : Rank
r4 =
    Rank 4


r5 : Rank
r5 =
    Rank 5


r6 : Rank
r6 =
    Rank 6


r7 : Rank
r7 =
    Rank 7


r8 : Rank
r8 =
    Rank 8
