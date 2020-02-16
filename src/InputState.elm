module InputState exposing (..)

import Field exposing (Field)
import File exposing (File)
import Parser exposing ((|.), (|=), Parser, backtrackable, end, oneOf, succeed)
import Piece exposing (Color(..), PieceType(..))
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
                |= Field.parser
                |. end
        , backtrackable <|
            succeed Moved
                |= Piece.parser
                |= Parser.map Just selectionHelperParser
                |= Field.parser
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
