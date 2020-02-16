module Piece exposing (Color(..), Field, Piece, PieceType(..), move, view)

import Field exposing (fileToX, rankToY)
import File exposing (File)
import Icons.Bishop
import Icons.King
import Icons.Knight
import Icons.Pawn
import Icons.Queen
import Icons.Rook
import Rank exposing (Rank)
import Svg exposing (Svg)


type alias Field =
    ( File, Rank )


type PieceType
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King


type alias Piece =
    { name : PieceType
    , color : Color
    , field : Field
    }


type Color
    = White
    | Black


move : List Piece -> Field -> Piece -> Maybe Piece
move otherPieces field piece =
    if isLegalMove otherPieces field piece then
        Just <|
            case piece.name of
                Pawn ->
                    if (Tuple.second >> Rank.toInt) field < 8 then
                        { piece | field = field }

                    else
                        { piece | name = Queen, field = field }

                _ ->
                    { piece | field = field }

    else
        Nothing


view : Int -> Piece -> Svg msg
view rotate piece =
    let
        ( file, rank ) =
            piece.field
    in
    case ( piece.name, piece.color ) of
        ( Pawn, White ) ->
            Icons.Pawn.white rotate (fileToX file) (rankToY rank)

        ( Pawn, Black ) ->
            Icons.Pawn.black rotate (fileToX file) (rankToY rank)

        ( King, White ) ->
            Icons.King.white rotate (fileToX file) (rankToY rank)

        ( King, Black ) ->
            Icons.King.black rotate (fileToX file) (rankToY rank)

        ( Queen, White ) ->
            Icons.Queen.white rotate (fileToX file) (rankToY rank)

        ( Queen, Black ) ->
            Icons.Queen.black rotate (fileToX file) (rankToY rank)

        ( Rook, White ) ->
            Icons.Rook.white rotate (fileToX file) (rankToY rank)

        ( Rook, Black ) ->
            Icons.Rook.black rotate (fileToX file) (rankToY rank)

        ( Knight, White ) ->
            Icons.Knight.white rotate (fileToX file) (rankToY rank)

        ( Knight, Black ) ->
            Icons.Knight.black rotate (fileToX file) (rankToY rank)

        ( Bishop, White ) ->
            Icons.Bishop.white rotate (fileToX file) (rankToY rank)

        ( Bishop, Black ) ->
            Icons.Bishop.black rotate (fileToX file) (rankToY rank)


isLegalMove : List Piece -> Field -> Piece -> Bool
isLegalMove otherPieces ( nextFile, nextRank ) piece =
    let
        ( prevFile, prevRank ) =
            piece.field

        fileDiff =
            File.toInt nextFile - File.toInt prevFile

        rankDiff =
            Rank.toInt nextRank - Rank.toInt prevRank

        legalMove =
            case piece.name of
                Rook ->
                    fileDiff == 0 && abs rankDiff > 0 || rankDiff == 0 && abs fileDiff > 0

                Bishop ->
                    abs fileDiff == abs rankDiff

                Knight ->
                    abs fileDiff == 2 && abs rankDiff == 1 || abs fileDiff == 1 && abs rankDiff == 2

                Queen ->
                    fileDiff == 0 && abs rankDiff > 0 || rankDiff == 0 && abs fileDiff > 0 || abs fileDiff == abs rankDiff

                King ->
                    (abs fileDiff == 1 && abs rankDiff == 1)
                        || (abs fileDiff == 0 && abs rankDiff == 1)
                        || (abs fileDiff == 1 && abs rankDiff == 0)

                Pawn ->
                    case piece.color of
                        White ->
                            let
                                hasEnemyPiece =
                                    List.any (\otherPiece -> otherPiece.field == ( nextFile, nextRank )) otherPieces
                            in
                            if not hasEnemyPiece then
                                if Rank.toInt prevRank == 2 then
                                    (fileDiff == 0) && (rankDiff == 1 || rankDiff == 2)

                                else
                                    (fileDiff == 0) && (rankDiff == 1)

                            else
                                (abs fileDiff == 1) && (rankDiff == 1)

                        Black ->
                            let
                                hasEnemyPiece =
                                    List.any (\otherPiece -> otherPiece.field == ( nextFile, nextRank )) otherPieces
                            in
                            if not hasEnemyPiece then
                                if Rank.toInt prevRank == 7 then
                                    (fileDiff == 0) && (rankDiff == -1 || rankDiff == -2)

                                else
                                    (fileDiff == 0) && (rankDiff == -1)

                            else
                                (abs fileDiff == 1) && (rankDiff == -1)

        steps =
            let
                fileSteps =
                    if piece.name == Knight then
                        []

                    else if fileDiff > 0 then
                        List.range (File.toInt prevFile + 1) (File.toInt nextFile - 1)

                    else if fileDiff < 0 then
                        List.range (File.toInt nextFile + 1) (File.toInt prevFile - 1) |> List.reverse

                    else
                        []

                rankSteps =
                    if piece.name == Knight then
                        []

                    else if rankDiff > 0 then
                        List.range (Rank.toInt prevRank + 1) (Rank.toInt nextRank - 1)

                    else if rankDiff < 0 then
                        List.range (Rank.toInt nextRank + 1) (Rank.toInt prevRank - 1) |> List.reverse

                    else
                        []

                tupleJoin ( maybeA, maybeB ) =
                    Maybe.map2 Tuple.pair maybeA maybeB
            in
            case ( fileSteps, rankSteps ) of
                ( [], _ ) ->
                    List.map (\rank -> ( Just prevFile, Rank.fromInt rank )) rankSteps
                        |> List.map tupleJoin

                ( _, [] ) ->
                    List.map (\file -> ( File.fromInt file, Just prevRank )) fileSteps
                        |> List.map tupleJoin

                _ ->
                    List.map2 Tuple.pair
                        (List.map File.fromInt fileSteps)
                        (List.map Rank.fromInt rankSteps)
                        |> List.map tupleJoin

        noBlockingPiece =
            List.all
                (\maybeStep ->
                    case maybeStep of
                        Nothing ->
                            False

                        Just step ->
                            List.all (\otherPiece -> otherPiece.field /= step) otherPieces
                )
                steps

        notTakesOwnPiece =
            not <|
                List.any
                    (\otherPiece ->
                        (piece.color == otherPiece.color)
                            && (( nextFile, nextRank ) == otherPiece.field)
                    )
                    otherPieces
    in
    legalMove && noBlockingPiece && notTakesOwnPiece
