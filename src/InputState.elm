module InputState exposing (..)

import Field exposing (Field)
import File exposing (File)
import Parser exposing ((|.), (|=), Parser, backtrackable, end, oneOf, succeed, symbol)
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank exposing (Rank)


type InputState
    = NotSelected
    | Selected PieceType (Maybe SelectionHelper)
    | Moved PieceType (Maybe SelectionHelper) Field


type SelectionHelper
    = WithFile File
    | WithRank Rank


selectionHelperParser : Parser SelectionHelper
selectionHelperParser =
    oneOf
        [ Parser.map WithFile File.parser
        , Parser.map WithRank Rank.parser
        ]


parser : Parser InputState
parser =
    oneOf
        [ succeed NotSelected
            |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= succeed Nothing
                |. oneOf [ succeed () |. symbol "x", succeed () ]
                |= Field.parser
                |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= Parser.map Just selectionHelperParser
                |= Field.parser
                |. oneOf [ succeed () |. symbol "x", succeed () ]
                |. end
        , backtrackable <|
            succeed Selected
                |= Piece.parser
                |= succeed Nothing
                |. end
        , succeed Selected
            |= Piece.parser
            |= Parser.map Just selectionHelperParser
            |. end
        ]