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
prettyPiece (Piece {pieceColor = White, pieceType = King})   = "♔"
prettyPiece (Piece {pieceColor = White, pieceType = Queen})  = "♕"
prettyPiece (Piece {pieceColor = White, pieceType = Rook})   = "♖"
prettyPiece (Piece {pieceColor = White, pieceType = Bishop}) = "♗"
prettyPiece (Piece {pieceColor = White, pieceType = Knight}) = "♘"
prettyPiece (Piece {pieceColor = White, pieceType = Pawn})   = "♙"
prettyPiece (Piece {pieceColor = Black, pieceType = King})   = "♚"
prettyPiece (Piece {pieceColor = Black, pieceType = Queen})  = "♛"
prettyPiece (Piece {pieceColor = Black, pieceType = Rook})   = "♜"
prettyPiece (Piece {pieceColor = Black, pieceType = Bishop}) = "♝"
prettyPiece (Piece {pieceColor = Black, pieceType = Knight}) = "♞"
prettyPiece (Piece {pieceColor = Black, pieceType = Pawn})   = "♟"

prettySquare :: Square -> String
prettySquare (Square _ Nothing)  = " "
prettySquare (Square _ (Just p)) = prettyPiece p

prettyBoard :: Board -> String
prettyBoard = (intercalate "\n")
            . (\s -> [prettySandwich' "┌" "┬" "┐"] ++ s ++ [prettySandwich' "└" "┴" "┘"])
            . (intersperse (prettySandwich' "├" "┼" "┤"))
            . (map (prettySandwich "│" "│" "│"))
            . (map (map prettySquare))
  where
      prettySandwich' = (\ a b c -> prettySandwich a b c (replicate 8 "─") )

