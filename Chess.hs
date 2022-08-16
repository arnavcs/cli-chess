module Chess where

import Data.List
import Data.Char
import Data.List.Index
import Control.Applicative

---------------------
-- DATA STRUCTURES --
---------------------

data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
    deriving (Read, Show, Eq)

data Color = White | Black
    deriving (Read, Show, Eq)

type Position = (Int, Int)

type Move = (Position, Position)

data Piece = Piece { pieceColor :: Color,
                      pieceType :: PieceType,
                       movedTwo :: Bool }
    deriving (Read, Show, Eq)

data Square = Square Position (Maybe Piece)
    deriving (Read, Show, Eq)

type Board = [[Square]]

data State = State {turn :: Color,
                   board :: Board}
    deriving (Read, Show, Eq)

--------------------
-- PRETTY DISPLAY --
--------------------

prettySandwich :: String -> String -> String -> [String] -> String
prettySandwich lc ic rc = (\ s -> lc ++ s ++ rc) . (intercalate ic)

prettyPosition :: Position -> String
prettyPosition (r, c) = (chr $ ord 'a' - 1 + r) : (intToDigit c) : []

prettyPiece :: Piece -> String
prettyPiece (Piece {pieceColor = White, pieceType = King  }) = "♔"
prettyPiece (Piece {pieceColor = White, pieceType = Queen }) = "♕"
prettyPiece (Piece {pieceColor = White, pieceType = Rook  }) = "♖"
prettyPiece (Piece {pieceColor = White, pieceType = Bishop}) = "♗"
prettyPiece (Piece {pieceColor = White, pieceType = Knight}) = "♘"
prettyPiece (Piece {pieceColor = White, pieceType = Pawn  }) = "♙"
prettyPiece (Piece {pieceColor = Black, pieceType = King  }) = "♚"
prettyPiece (Piece {pieceColor = Black, pieceType = Queen }) = "♛"
prettyPiece (Piece {pieceColor = Black, pieceType = Rook  }) = "♜"
prettyPiece (Piece {pieceColor = Black, pieceType = Bishop}) = "♝"
prettyPiece (Piece {pieceColor = Black, pieceType = Knight}) = "♞"
prettyPiece (Piece {pieceColor = Black, pieceType = Pawn  }) = "♟"

prettyBoard :: Board -> String
prettyBoard = (intercalate "\n")
            . (\ s -> [prettySandwich' "┌" "┬" "┐"] ++ s ++ [prettySandwich' "└" "┴" "┘"])
            . (intersperse (prettySandwich' "├" "┼" "┤"))
            . (map (prettySandwich "│" "│" "│"))
            . (map (map prettySquare))
  where
    prettySandwich' :: String -> String -> String -> String
    prettySandwich' = (\ a b c -> prettySandwich a b c (replicate 8 "───") )

    prettySquare :: Square -> String
    prettySquare (Square _ Nothing)  = "   "
    prettySquare (Square _ (Just p)) = " " ++ prettyPiece p ++ " "

-----------------
-- DEFINITIONS --
-----------------

initialBoard :: Board
initialBoard = [[Square (1, 8) (jp Black Rook), Square (2, 8) (jp Black Knight), Square (3, 8) (jp Black Bishop), Square (4, 8) (jp Black Queen), Square (5, 8) (jp Black King), Square (6, 8) (jp Black Bishop), Square (7, 8) (jp Black Knight), Square (8, 8) (jp Black Rook)]
              , [Square (1, 7) (jp Black Pawn), Square (2, 7) (jp Black Pawn)  , Square (3, 7) (jp Black Pawn)  , Square (4, 7) (jp Black Pawn) , Square (5, 7) (jp Black Pawn), Square (6, 7) (jp Black Pawn)  , Square (7, 7) (jp Black Pawn)  , Square (8, 7) (jp Black Pawn)]
              , [Square (1, 6) Nothing        , Square (2, 6) Nothing          , Square (3, 6) Nothing          , Square (4, 6) Nothing         , Square (5, 6) Nothing        , Square (6, 6) Nothing          , Square (7, 6) Nothing          , Square (8, 6) Nothing        ]
              , [Square (1, 5) Nothing        , Square (2, 5) Nothing          , Square (3, 5) Nothing          , Square (4, 5) Nothing         , Square (5, 5) Nothing        , Square (6, 5) Nothing          , Square (7, 5) Nothing          , Square (8, 5) Nothing        ]
              , [Square (1, 4) Nothing        , Square (2, 4) Nothing          , Square (3, 4) Nothing          , Square (4, 4) Nothing         , Square (5, 4) Nothing        , Square (6, 4) Nothing          , Square (7, 4) Nothing          , Square (8, 4) Nothing        ]
              , [Square (1, 3) Nothing        , Square (2, 3) Nothing          , Square (3, 3) Nothing          , Square (4, 3) Nothing         , Square (5, 3) Nothing        , Square (6, 3) Nothing          , Square (7, 3) Nothing          , Square (8, 3) Nothing        ]
              , [Square (1, 2) (jp White Pawn), Square (2, 2) (jp White Pawn)  , Square (3, 2) (jp White Pawn)  , Square (4, 2) (jp White Pawn) , Square (5, 2) (jp White Pawn), Square (6, 2) (jp White Pawn)  , Square (7, 2) (jp White Pawn)  , Square (8, 2) (jp White Pawn)]
              , [Square (1, 1) (jp White Rook), Square (2, 1) (jp White Knight), Square (3, 1) (jp White Bishop), Square (4, 1) (jp White Queen), Square (5, 1) (jp White King), Square (6, 1) (jp White Bishop), Square (7, 1) (jp White Knight), Square (8, 1) (jp White Rook)]]
  where
    jp color pType = Just $ Piece {pieceColor = color, pieceType = pType, movedTwo = False}

pieceValue :: Piece -> Int
pieceValue (Piece {pieceType = Pawn})   = 1
pieceValue (Piece {pieceType = Rook})   = 5
pieceValue (Piece {pieceType = Knight}) = 3
pieceValue (Piece {pieceType = Bishop}) = 3
pieceValue (Piece {pieceType = Queen})  = 9
pieceValue (Piece {pieceType = King})   = 0

isValidPosition :: Position -> Bool
isValidPosition = uncurry (&&) . both (\a -> a >= 1 && a <= 8)
  where
    both :: (a -> b) -> (a, a) -> (b, b)
    both f (x, y) = (f x, f y)

boardValue :: Color -> Board -> Int
boardValue c = sum . map (\ (Square _ mp) -> maybe 0 colorPieceValue mp) . concat
  where
    colorPieceValue :: Piece -> Int
    colorPieceValue p
        | pieceColor p == c = pieceValue p
        | otherwise         = 0

boardEvaluation :: Board -> Int
boardEvaluation = liftA2 (-) (boardValue White) (boardValue Black)

colorPiecePositions :: Color -> Board -> [Position]
colorPiecePositions c = map (\ (Square p _) -> p)
                      . filter (\ (Square _ p) -> maybe False correctColor p)
                      . concat
  where
    correctColor :: Piece -> Bool
    correctColor p = c == pieceColor p

getSquare :: Position -> Board -> Square
getSquare (r, c) = (!! (c - 1)) . (!! (8 - r))

-- isValidMove :: Move -> Bool
-- 
-- makeMove :: Move -> Board -> Board

changeSquare :: Position -> Maybe Piece -> Board -> Board
changeSquare (r, c) mp = (modifyAt (8 - r)
                         (modifyAt (c - 1)
                         (\ (Square p _) -> Square p mp)))
