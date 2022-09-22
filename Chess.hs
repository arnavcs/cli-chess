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
          | EPMove Position Position
          | CMove Position Position
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

data PieceQuery = AllQuery
                | TypeQuery PieceType
                | ColorQuery Color
                | MixedQuery Color PieceType
    deriving (Read, Show, Eq)

-----------------------
-- PRETTY DISPLAYING --
-----------------------

-- returns the position as a string
prettyPosition :: Position -> String
prettyPosition (Position (r, c)) = (chr $ ord 'a' + c) : (intToDigit (8 - r)) : []

-- returns the piece as a string
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

-- returns the square as a string
prettySquare :: Square -> String
prettySquare Nothing  = "   "
prettySquare (Just p) = " " ++ prettyPiece p ++ " "

-- intersperses the given string list with left, right, and in-between decorators
prettySandwich :: String -> String -> String -> [String] -> String
prettySandwich lc ic rc = (\ s -> lc ++ s ++ rc) . (intercalate ic)

-- returns the board as a string
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

-- returns the square at the given position on the board
getSquare :: Position -> Board -> Square
getSquare (Position (r, c)) = (!! c) . (!! r)

-- sets the square at the given position on the board
setSquare :: Position -> Square -> Board -> Board
setSquare (Position (r, c)) s = modifyAt r (setAt c s)

-- returns if there is a piece on the given position of the board
isPiece :: Position -> Bool
isPiece = (/= Nothing) . (flip getSquare b)

----------------------
-- QUERY FULFILLING --
----------------------

-- returns a piece which satisfies the query supplied
tempPiece :: PieceQuery -> Piece
tempPiece (AllQuery)         = Piece {pieceColor = White, pieceType = Pawn, moved = False, justMovedTwo = False}
tempPiece (TypeQuery pt)     = Piece {pieceColor = White, pieceType = pt  , moved = False, justMovedTwo = False}
tempPiece (ColorQuery pc)    = Piece {pieceColor = pc   , pieceType = Pawn, moved = False, justMovedTwo = False}
tempPiece (MixedQuery pc pt) = Piece {pieceColor = pc   , pieceType = pt  , moved = False, justMovedTwo = False}

-- checks if a piece satisfies the query
satisfiesQuery :: Piece -> PieceQuery -> Bool
satisfiesQuery p (AllQuery)         = True
satisfiesQuery p (TypeQuery pt)     = pieceType p  == pt
satisfiesQuery p (ColorQuery pc)    = pieceColor p == pc
satisfiesQuery p (MixedQuery pc pt) = pieceType p  == pt && pieceColor p == pc

-- returns all possible board positions
allPositions :: [Position]
allPositions = map Position $ (,) <$> [0..7] <*> [0..7]

-- returns the position of all pieces satisfying the query
piecePositions :: PieceQuery -> Board -> [Position]
piecePositions q b = filter ((maybe False (`satisfiesQuery` q))
                           . (flip getSquare b))
                   $ allPositions

-------------------
-- INITIAL BOARD --
-------------------

-- the board setup at the beginning of the game
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

------------------------
-- POSITIONS ATTACKED --
------------------------

-- returns 7 tuples that are elementwise integer multiples of the supplied tuple
getSeven :: (Int, Int) -> [(Int, Int)]
getSeven (a, b) = map (\ n -> (n * a, n * b)) . take 7 $ [1..]

-- returns the total attack range of the piece in question
-- tuples that come later in the same list as another tuple cannot be moved to if one cannot move to an earlier tuple in the list
pieceAttackingRange :: Piece -> [[(Int, Int)]]
pieceAttackingRange Piece {pieceType = Pawn, pieceColor = White} = [[(-1, -1)], [(-1, 1)]]
pieceAttackingRange Piece {pieceType = Pawn, pieceColor = Black} = [[(1 , -1)], [(1 , 1)]]
pieceAttackingRange Piece {pieceType = Rook  } = map getSeven [(0, -1), (0, 1), (-1, 0), (1, 0)]
pieceAttackingRange Piece {pieceType = Knight} = map (\ [a, b] -> [(a, b)]) . concat . map permutations $ (++) <$> [[1], [-1]] <*> [[2], [-2]]
pieceAttackingRange Piece {pieceType = Bishop} = map getSeven [(1, 1), (1, -1), (-1, 1), (-1, -1)]
pieceAttackingRange Piece {pieceType = Queen } = pieceAttackingRange (tempPiece $ TypeQuery Rook) ++ pieceAttackingRange (tempPiece $ TypeQuery Bishop)
pieceAttackingRange Piece {pieceType = King  } = map (\ c -> [head c]) . pieceAttackingRange $ tempPiece (TypeQuery Queen)

-- returns a position shifted by the given tuple, or nothing
shiftPosition :: Position -> (Int, Int) -> Maybe Position
shiftPosition (Position (a, b)) (da, db)
  | isInvalid (a + da, b + db) = Nothing
  | otherwise                  = Just $ Position (a + da, b + db)
  where
    isInvalid :: (Int, Int) -> Bool
    isInvalid (m, n) = not $ 0 <= m && m <= 7 && 0 <= n && n <= 7

-- returns the list of postions the piece at a given board position attacks (cannot be a piece of the same color)
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

    combineBroken :: ([Position], [Position]) -> [Position]
    combineBroken (ns, []) = ns
    combineBroken (ns, m:ms) = if (\ [r, s] -> r == s)
                                . map (fmap pieceColor . flip getSquare b)
                                $ [m, p]
                               then ns
                               else ns ++ [m]

-- returns if the current position is being attacked by another piece
isUnderAttack :: Position -> Board -> Bool
isUnderAttack p b = elem p . concat . map (flip attackingPositions b) $ allPositions

-----------------------
-- DETERMINING CHECK --
-----------------------

-- checks if the king of the color is beign attacked
isUnderCheck :: Color -> Board -> Bool
isUnderCheck c b = isUnderAttack (head $ piecePositions (MixedQuery c King) b) b

----------------------
-- MOVE INTERACTION --
----------------------

-- returns the position of the piece taken during enpassant
enpassantPosition :: Position -> Position -> Position
enpassantPosition (Position (r, _)) (Position (_, c)) = Position (r, c)

-- returns the position of the rook initially and at the end of the castle
castlingPositions :: Position -> Position -> (Position, Position)
castlingPositions (Position (r, c1)) (Position (_, c2))
  | c1 < c2   = (Position (r, 7), Position (r, 5))
  | otherwise = (Position (r, 0), Position (r, 3))

-- returns the board after naively making the move described
-- todo: change the "moved" values of the pieces that are moved
-- todo: add a move2 move for pawns
makeMoveUnsafe :: Move -> Board -> Board
makeMoveUnsafe (SMove  pi pf)   b = setSquare pi Nothing
                                  . setSquare pf (getSquare pi b)
                                  $ b
makeMoveUnsafe (EPMove pi pf)   b = let pc = enpassantPosition pi pf
                                     in setSquare pi Nothing
                                      . setSquare pc Nothing
                                      . setSquare pf (getSquare pi b)
                                      $ b
makeMoveUnsafe (CMove  pi pf)   b = let (pe, pr) = castlingPositions pi pf
                                     in setSquare pi Nothing
                                      . setSquare pe Nothing
                                      . setSquare pf (getSquare pi b)
                                      . setSquare pr (getSquare pe b)
                                      $ b
makeMoveUnsafe (PMove  pi pf p) b = let c = maybe White pieceColor $ getSquare pi b
                                     in setSquare pi Nothing
                                      . setSquare pf (Just $ tempPiece (MixedQuery c p))
                                      $ b

-- returns the valid moves for a given piece (including EPMove, SMove, CMove, and PMove)
-- todo: write this function
getMoves :: Position -> Board -> [Move]
getMoves p b = []
-- getMoves p b = maybe Nothing pieceMoves $ getSquare p b
--   where
--     pieceMoves :: Piece -> [Move]
--     pieceMoves p@(Piece {pieceType = Pawn}) =

-- returns all the valid moves for the queried pieces
getAllQueriedMoves :: PieceQuery -> Board -> [Move]
getAllQueriedMoves pq b = concat . map (flip getMoves b) $ piecePositions pq b

-- returns all possible moves for the current game state
-- todo: write this function
getStateMoves :: State -> [Move]

-- safely makes the move if it is valid, otherwise it returns nothing
-- todo: write this function
makeMove :: Move -> State -> Maybe State
