module Chess where

import Data.List

data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
    deriving (Read, Show, Eq)

data Color = White | Black
    deriving (Read, Show, Eq)

type Position = (Int, Int)

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

prettySandwich :: String -> String -> String -> [String] -> String
prettySandwich lc ic rc = (\s -> lc ++ s ++ rc) . (intercalate ic)

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
            . (\s -> [prettySandwich' "┌" "┬" "┐"] ++ s ++ [prettySandwich' "└" "┴" "┘"])
            . (intersperse (prettySandwich' "├" "┼" "┤"))
            . (map (prettySandwich "│" "│" "│"))
            . (map (map prettySquare))
  where
    prettySandwich' :: String -> String -> String -> String
    prettySandwich' = (\ a b c -> prettySandwich a b c (replicate 8 "───") )

    prettySquare :: Square -> String
    prettySquare (Square _ Nothing)  = "   "
    prettySquare (Square _ (Just p)) = " " ++ prettyPiece p ++ " "

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

