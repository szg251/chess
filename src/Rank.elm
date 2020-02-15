module Rank exposing (Rank, fromInt, r1, r2, r3, r4, r5, r6, r7, r8, toInt)


type Rank
    = Rank Int


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
