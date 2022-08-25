module Chess where

import Data.List
import Data.Char
import Data.Maybe
import Data.List.Index
import Control.Applicative

----------------------
-- TYPE DEFINITIONS --
----------------------

data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
    deriving (Read, Show, Eq)

data Color = White | Black
    deriving (Read, Show, Eq)

newtype Position = Position (Int, Int)
    deriving (Read, Show, Eq)

data Move = SMove Position Position
          | PMove Position Position PieceType
    deriving (Read, Show, Eq)

data Piece = Piece { pieceColor :: Color,
                      pieceType :: PieceType,
                          moved :: Bool,
                   justMovedTwo :: Bool }
    deriving (Read, Show, Eq)

type Square = Maybe Piece

type Board = [[Square]]

data State = State {turn :: Color,
                   board :: Board}
    deriving (Read, Show, Eq)

data PieceQuery = TypeQuery PieceType
                | ColorQuery Color
                | MixedQuery Color PieceType
    deriving (Read, Show, Eq)

-----------------------
-- PRETTY DISPLAYING --
-----------------------

prettyPosition :: Position -> String
prettyPosition (Position (r, c)) = (chr $ ord 'a' + c) : (intToDigit (8 - r)) : []

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

prettySquare :: Square -> String
prettySquare Nothing  = "   "
prettySquare (Just p) = " " ++ prettyPiece p ++ " "

prettySandwich :: String -> String -> String -> [String] -> String
prettySandwich lc ic rc = (\ s -> lc ++ s ++ rc) . (intercalate ic)

prettyBoard :: Board -> String
prettyBoard = (intercalate "\n")
            . (\ s -> [prettySandwich' "┌" "┬" "┐"] ++ s ++ [prettySandwich' "└" "┴" "┘"])
            . (intersperse (prettySandwich' "├" "┼" "┤"))
            . (map (prettySandwich "│" "│" "│"))
            . (map (map prettySquare))
  where
    prettySandwich' :: String -> String -> String -> String
    prettySandwich' = (\ a b c -> prettySandwich a b c (replicate 8 "───") )

---------------------
-- BOARD ACCESSING --
---------------------

getSquare :: Position -> Board -> Square
getSquare (Position (r, c)) = (!! c) . (!! r)

----------------------
-- QUERY FULFILLING --
----------------------

tempPiece :: PieceQuery -> Piece
tempPiece (TypeQuery pt)     = Piece {pieceColor = White, pieceType = pt  , moved = False, justMovedTwo = False}
tempPiece (ColorQuery pc)    = Piece {pieceColor = pc   , pieceType = Pawn, moved = False, justMovedTwo = False}
tempPiece (MixedQuery pc pt) = Piece {pieceColor = pc   , pieceType = pt  , moved = False, justMovedTwo = False}

satisfiesQuery :: Piece -> PieceQuery -> Bool
satisfiesQuery p (TypeQuery pt)     = pieceType p  == pt
satisfiesQuery p (ColorQuery pc)    = pieceColor p == pc
satisfiesQuery p (MixedQuery pc pt) = pieceType p  == pt && pieceColor p == pc

piecePositions :: PieceQuery -> Board -> [Position]
piecePositions q b = filter ((maybe False (`satisfiesQuery` q))
                           . (flip getSquare b))
                   . map Position
                   $ (,) <$> [0..7] <*> [0..7]

-------------------
-- INITIAL BOARD --
-------------------

initialBoard :: Board
initialBoard = [[jp Black Rook, jp Black Knight, jp Black Bishop, jp Black Queen, jp Black King, jp Black Bishop, jp Black Knight, jp Black Rook]
              , [jp Black Pawn, jp Black Pawn  , jp Black Pawn  , jp Black Pawn , jp Black Pawn, jp Black Pawn  , jp Black Pawn  , jp Black Pawn]
              , [Nothing      , Nothing        , Nothing        , Nothing       , Nothing      , Nothing        , Nothing        , Nothing      ]
              , [Nothing      , Nothing        , Nothing        , Nothing       , Nothing      , Nothing        , Nothing        , Nothing      ]
              , [Nothing      , Nothing        , Nothing        , Nothing       , Nothing      , Nothing        , Nothing        , Nothing      ]
              , [Nothing      , Nothing        , Nothing        , Nothing       , Nothing      , Nothing        , Nothing        , Nothing      ]
              , [jp White Pawn, jp White Pawn  , jp White Pawn  , jp White Pawn , jp White Pawn, jp White Pawn  , jp White Pawn  , jp White Pawn]
              , [jp White Rook, jp White Knight, jp White Bishop, jp White Queen, jp White King, jp White Bishop, jp White Knight, jp White Rook]]
  where
    jp :: Color -> PieceType -> Square
    jp pc pt = Just $ tempPiece (MixedQuery pc pt)

----------------------
-- VALUE EVALUATION --
----------------------

pieceValue :: Piece -> Int
pieceValue (Piece {pieceType = Pawn})   = 1
pieceValue (Piece {pieceType = Rook})   = 5
pieceValue (Piece {pieceType = Knight}) = 3
pieceValue (Piece {pieceType = Bishop}) = 3
pieceValue (Piece {pieceType = Queen})  = 9
pieceValue (Piece {pieceType = King})   = 0

queryValue :: PieceQuery -> Board -> Int
queryValue q b = sum
               . map ((maybe 0 pieceValue) . (flip getSquare b))
               . piecePositions q
               $ b

boardEvaluation :: Board -> Int
boardEvaluation = liftA2 (-) (queryValue $ ColorQuery White) (queryValue $ ColorQuery Black)

------------------------
-- POSITIONS ATTACKED --
------------------------

getSeven :: (Int, Int) -> [(Int, Int)]
getSeven (a, b) = map (\ n -> (n * a, n * b)) . take 7 $ [1..]

pieceAttackingRange :: Piece -> [[(Int, Int)]]
pieceAttackingRange Piece {pieceType = Pawn, pieceColor = White} = [[(-1, -1)], [(-1, 1)]]
pieceAttackingRange Piece {pieceType = Pawn, pieceColor = Black} = [[(1 , -1)], [(1 , 1)]]
pieceAttackingRange Piece {pieceType = Rook  } = map getSeven [(0, -1), (0, 1), (-1, 0), (1, 0)]
pieceAttackingRange Piece {pieceType = Knight} = map (\ [a, b] -> [(a, b)]) . concat . map permutations $ (++) <$> [[1], [-1]] <*> [[2], [-2]]
pieceAttackingRange Piece {pieceType = Bishop} = map getSeven [(1, 1), (1, -1), (-1, 1), (-1, -1)]
pieceAttackingRange Piece {pieceType = Queen } = pieceAttackingRange (tempPiece $ TypeQuery Rook) ++ pieceAttackingRange (tempPiece $ TypeQuery Bishop)
pieceAttackingRange Piece {pieceType = King  } = map (\ c -> [head c]) . pieceAttackingRange $ tempPiece (TypeQuery Queen)

shiftPosition :: Position -> (Int, Int) -> Maybe Position
shiftPosition (Position (a, b)) (da, db)
  | isInvalid (a + da, b + db) = Nothing
  | otherwise                  = Just $ Position (a + da, b + db)
  where
    isInvalid :: (Int, Int) -> Bool
    isInvalid (m, n) = not $ 0 <= m && m <= 7 && 0 <= n && n <= 7

attackingPositions :: Position -> Board -> [Position]
attackingPositions p b = maybe [] attackingPositionsHelper $ getSquare p b
  where
    attackingPositionsHelper :: Piece -> [Position]
    attackingPositionsHelper = concat
                             . map (combineBroken
                                  . break isPiece
                                  . catMaybes
                                  . map (shiftPosition p))
                             . pieceAttackingRange

    combineBroken :: ([a], [a]) -> [a]
    combineBroken (as, []) = as
    combineBroken (as, b:bs) = as ++ [b]

    isPiece :: Position -> Bool
    isPiece = (/= Nothing) . (flip getSquare b)

-----------------------
-- DETERMINING CHECK --
-----------------------

nextColor :: Color -> Color
nextColor White = Black
nextColor Black = White

isUnderCheck :: Color -> Board -> Bool
isUnderCheck c b = elem (head $ piecePositions (MixedQuery c King) b)
                 . concat
                 . map (flip attackingPositions b)
                 . piecePositions (ColorQuery $ nextColor c)
                 $ b


