module GameLogic exposing (..)

import Field exposing (Field)
import File
import InputState exposing (InputState(..), SelectionHelper(..))
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank
import Result.Extra as ResultE


move : List Piece -> Field -> Piece -> Result String (List Piece)
move pieces field selected =
    let
        otherPieces =
            List.filter (\piece -> piece /= selected) pieces

        remainedPieces =
            List.filter (\piece -> piece.field /= field) otherPieces
    in
    if isLegalMove otherPieces field selected then
        case selected.name of
            Pawn ->
                if (Tuple.second >> Rank.toInt) field < 8 then
                    Ok <| { selected | field = field } :: remainedPieces

                else
                    Ok <| { selected | name = Queen, field = field } :: remainedPieces

            _ ->
                Ok <| { selected | field = field } :: remainedPieces

    else
        Err "This move is not possible."


movePiece : InputState -> Color -> List Piece -> Result String (List Piece)
movePiece inputState turn pieces =
    case inputState of
        Moved _ _ field ->
            let
                attempts =
                    InputState.getSelectedPieces inputState turn pieces
                        |> List.map (move pieces field)
                        |> ResultE.partition
            in
            case attempts of
                ( [ onePossibleMove ], _ ) ->
                    Ok onePossibleMove

                ( [], err :: _ ) ->
                    Err err

                _ ->
                    Err "Multiple possible moves. Try to specify which file or rank your piece is on. Ex. Rd5 -> Rad5"

        _ ->
            Ok pieces


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
