module Engine where

import Data.List
import Data.Maybe

import Chess

---------------------
-- DATA DEFINITONS --
---------------------

data GameTree = GameNode State [GameTree]
              | GameLeaf State
    deriving (read, show, eq)

----------------------
-- VALUE EVALUATION --
----------------------

-- the value of a given piece
pieceValue :: Piece -> Int
pieceValue (Piece {pieceType = Pawn})   = 1
pieceValue (Piece {pieceType = Rook})   = 5
pieceValue (Piece {pieceType = Knight}) = 3
pieceValue (Piece {pieceType = Bishop}) = 3
pieceValue (Piece {pieceType = Queen})  = 9
pieceValue (Piece {pieceType = King})   = 0

-- the total value of the pieces satifying the query
queryValue :: PieceQuery -> Board -> Int
queryValue q b = sum
               . map ((maybe 0 pieceValue) . (flip getSquare b))
               . piecePositions q
               $ b

-- returns the difference in value on the board between the white and black player
boardEvaluation :: Board -> Int
boardEvaluation = liftA2 (-) (queryValue $ ColorQuery White) (queryValue $ ColorQuery Black)

-- returns the relative value of the board for the colour specified
stateEvaluation :: State -> Int
stateEvaluation s = (if turn s == White then id else negate) . boardEvaluation $ board s

--------------------------
-- GENERATING GAME TREE --
--------------------------


