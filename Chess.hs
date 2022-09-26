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
          | TMove Position
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
                | MovedTwoQuery Bool
                | MovedQuery Bool
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

-- returns true if there is a piece on the given position of the board
isPiece :: Position -> Board -> Bool
isPiece p b = (/= Nothing) $ getSquare p b

-- returns true if all positions have a piece on the board
arePieces :: [Position] -> Board -> Bool
arePieces ps b = and . map (flip isPiece b) $ ps

-- returns true if there is no piece on the given position of the board
isEmpty :: Position -> Board -> Bool
isEmpty p b = not $ isPiece p b

-- returns true if all positions don't have a piece on the board
areEmpty :: [Position] -> Board -> Bool
areEmpty ps b = and . map (flip isEmpty b) $ ps

----------------------
-- QUERY FULFILLING --
----------------------

-- returns a piece which satisfies the query supplied
tempPiece :: PieceQuery -> Piece
tempPiece (AllQuery)         = Piece {pieceColor = White, pieceType = Pawn, moved = False, justMovedTwo = False}
tempPiece (TypeQuery pt)     = Piece {pieceColor = White, pieceType = pt  , moved = False, justMovedTwo = False}
tempPiece (ColorQuery pc)    = Piece {pieceColor = pc   , pieceType = Pawn, moved = False, justMovedTwo = False}
tempPiece (MovedTwoQuery mt) = Piece {pieceColor = White, pieceType = Pawn, moved = False, justMovedTwo = mt   }
tempPiece (MovedQuery pm)    = Piece {pieceColor = White, pieceType = Pawn, moved = pm   , justMovedTwo = False}
tempPiece (MixedQuery pc pt) = Piece {pieceColor = pc   , pieceType = pt  , moved = False, justMovedTwo = False}

-- checks if a piece satisfies the query
satisfiesQuery :: Piece -> PieceQuery -> Bool
satisfiesQuery p (AllQuery)         = True
satisfiesQuery p (TypeQuery pt)     = pieceType p    == pt
satisfiesQuery p (ColorQuery pc)    = pieceColor p   == pc
satisfiesQuery p (MovedTwoQuery mt) = justMovedTwo p == mt
satisfiesQuery p (MovedQuery pm)    = moved p        == pm
satisfiesQuery p (MixedQuery pc pt) = pieceType p    == pt
                                   && pieceColor p   == pc

-- checks if the piece on a position satifies the query
posSatisfiesQuery :: Position -> Board -> PieceQuery -> Bool
posSatisfiesQuery p b pq = maybe False (flip satisfiesQuery pq) $ getSquare p b

-- returns all possible board positions
allPositions :: [Position]
allPositions = map Position $ (,) <$> [0..7] <*> [0..7]

-- returns the position of all pieces satisfying the query
piecePositions :: PieceQuery -> Board -> [Position]
piecePositions q b = filter ((maybe False (`satisfiesQuery` q))
                           . (flip getSquare b))
                   $ allPositions

------------------------
-- PIECE MANIPULATION --
------------------------

-- returns an identical piece where the moved field is true and the justMovedTwo field is false
makeMoved :: Piece -> Piece
makeMoved p = p {moved = True, justMovedTwo = False}

-- returns an identical piece where the justMovedTwo and moved fields are true
makeJustMovedTwo :: Piece -> Piece
makeJustMovedTwo p = p {moved = True, justMovedTwo = True}

-------------------
-- INITIAL SETUP --
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

-- the initial game state at the beginning of the game
initialState :: State
initialState = State {turn  = White,
                      board = initialBoard}

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
                                  . break (flip isPiece b)
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

-- returns if the current position is being attacked by another piece that satifies the given query
isUnderAttackByQuery :: Position -> Board -> PieceQuery -> Bool
isUnderAttackByQuery p b pq = elem p . concat . map (flip attackingPositions b) $ piecePositions pq b

-- returns if the current position is being attacked by another piece
isUnderAttack :: Position -> Board -> Bool
isUnderAttack p b = isUnderAttackByQuery p b AllQuery

-----------------------
-- DETERMINING CHECK --
-----------------------

-- checks if the king of the color is beign attacked
isUnderCheck :: Color -> Board -> Bool
isUnderCheck c b = isUnderAttack (head $ piecePositions (MixedQuery c King) b) b

----------------------
-- MOVE INTERACTION --
----------------------

jumpTwoPosition :: Position -> Position
jumpTwoPosition (Position (r, c))
  | r < 4     = Position (3, r)
  | otherwise = Position (4, r)

-- returns the position of the piece taken during enpassant
enpassantPosition :: Position -> Position -> Position
enpassantPosition (Position (r, _)) (Position (_, c)) = Position (r, c)

-- returns the position of the rook initially and at the end of the castle
castlingPositions :: Position -> Position -> (Position, Position)
castlingPositions (Position (r, c1)) (Position (_, c2))
  | c1 < c2   = (Position (r, 7), Position (r, 5))
  | otherwise = (Position (r, 0), Position (r, 3))

-- returns the board after naively making the move described
makeMoveUnsafe :: Move -> Board -> Board
makeMoveUnsafe (SMove  pi pf)   b = setSquare pi Nothing
                                  . setSquare pf (fmap makeMoved $ getSquare pi b)
                                  $ b
makeMoveUnsafe (TMove  pi)      b = let pf = jumpTwoPosition pi
                                     in setSquare pi Nothing
                                      . setSquare pf (fmap makeJustMovedTwo $ getSquare pi b)
                                      $ b
makeMoveUnsafe (EPMove pi pf)   b = let pc = enpassantPosition pi pf
                                     in setSquare pi Nothing
                                      . setSquare pc Nothing
                                      . setSquare pf (fmap makeMoved $ getSquare pi b)
                                      $ b
makeMoveUnsafe (CMove  pi pf)   b = let (pe, pr) = castlingPositions pi pf
                                     in setSquare pi Nothing
                                      . setSquare pe Nothing
                                      . setSquare pf (fmap makeMoved $ getSquare pi b)
                                      . setSquare pr (fmap makeMoved $ getSquare pe b)
                                      $ b
makeMoveUnsafe (PMove  pi pf p) b = let c = maybe White pieceColor $ getSquare pi b
                                     in setSquare pi Nothing
                                      . setSquare pf (Just $ makeMoved . tempPiece $ MixedQuery c p)
                                      $ b

-- returns the next color to play
otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black

-- returns the valid moves for a given piece (including EPMove, SMove, CMove, and PMove)
-- TODO: reformatting
validMoves :: Position -> Board -> [Move]
validMoves p b = maybe [] (\ p' -> filter ( not
                                          . isUnderCheck (pieceColor p')
                                          . (flip makeMoveUnsafe b) )
                                 $ pieceMoves p')
               $ getSquare p b
  where
    pieceMoves :: Piece -> [Move]
    pieceMoves p'@(Piece {pieceType = Pawn}) = ( filter (const . hasMoved False $ p) -- TMove
                                               . map (\x -> TMove p)
                                               . filter (flip areEmpty b)
                                               . catMaybes
                                               . map sequence
                                               . map (map (shiftPosition p))
                                               $ if pieceColor p' == White then [[(-1, 0), (-2, 0)]] else [[(1, 0), (2, 0)]] )
                                            ++ ( concat                            -- SMove and PMove for pawns (no capturing)
                                               . map (flip getPawnPSMoves $ pieceColor p')
                                               . catMaybes
                                               . map (shiftPosition p)
                                               $ if pieceColor p' == White then [(-1, 0)] else [(1, 0)] )
                                            ++ ( concat                            -- SMove and PMove for pawns (with capturing)
                                               . map (flip getPawnPSMoves $ pieceColor p')
                                               . filter (isColor $ otherColor (pieceColor p'))
                                               . catMaybes
                                               . map (shiftPosition p)
                                               $ if pieceColor p' == White then [(-1, -1), (-1, 1)] else [(1, -1), (1, 1)] )
                                            ++ ( map (\ x -> EPMove p x)           -- EPMove
                                               . filter (\ x -> isEmpty x b
                                                             && isColor (otherColor . pieceColor $ p') (enpassantPosition p x)
                                                             && posSatisfiesQuery (enpassantPosition p x) b (MovedTwoQuery True))
                                               . catMaybes
                                               . map (shiftPosition p)
                                               $ if pieceColor p' == White then [(-1, -1), (-1, 1)] else [(1, -1), (1, 1)] )
    pieceMoves p'@(Piece {pieceType = King}) = ( map (\ x -> SMove p x) $ attackingPositions p b ) -- SMove
                                            ++ ( map (\ x -> CMove p x)
                                               . filter ( and
                                                        . map (\ p -> not $ isUnderAttackByQuery p b (ColorQuery $ otherColor (pieceColor p')))
                                                        . (\ x -> [p, x, fst $ castlingPositions p x, snd $ castlingPositions p x]))
                                               . filter (hasMoved False . fst . castlingPositions p)
                                               . filter (const . hasMoved False $ p)
                                               . filter (const $ (p == Position (7, 4) || p == Position (0, 4)))
                                               $ if pieceColor p' == White then [Position (7, 2), Position (7, 6)] else [Position (0, 2), Position (0, 6)] )
    pieceMoves p'                            = map (\ x -> SMove p x) $ attackingPositions p b

    -- returns true if the given relative positions are all empty and valid positions
    areEmptyRel :: [(Int, Int)] -> Bool
    areEmptyRel = maybe False (flip areEmpty b) . sequence . map (shiftPosition p)

    -- returns true if the given position has a piece of the given color
    isColor :: Color -> Position -> Bool
    isColor c p = posSatisfiesQuery p b (ColorQuery c)

    -- returns true if the piece on the given position has the same moved value as the parameter
    hasMoved :: Bool -> Position -> Bool
    hasMoved m p = posSatisfiesQuery p b (MovedQuery m)

    -- returns a list of possible promotion or standard moves when given the final position of the move
    getPawnPSMoves :: Position -> Color -> [Move]
    getPawnPSMoves x@(Position (r, c)) c' = if r == (if c' == White then 0 else 7) then allPromotions p x else [SMove p x]

    -- given the initial and final position of the move, returns all possible promotion moves
    allPromotions :: Position -> Position -> [Move]
    allPromotions pi pf = map (\ x -> PMove pi pf x) [Rook, Knight, Bishop, Queen]

-- returns all the valid moves for the queried pieces
allQueriedPieceMoves :: PieceQuery -> Board -> [Move]
allQueriedPieceMoves pq b = concat . map (flip validMoves b) $ piecePositions pq b

-- returns all possible moves for the current game state
stateMoves :: State -> [Move]
stateMoves s = allQueriedPieceMoves (ColorQuery $ turn s) $ board s

-- safely makes the move if it is valid, otherwise it returns nothing
makeMove :: Move -> State -> Maybe State
makeMove m s = if notElem m $ stateMoves s
             then Nothing
             else Just $ s {turn  = otherColor $ turn s,
                            board = makeMoveUnsafe m $ board s}
