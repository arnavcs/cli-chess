module Engine where

import Chess

zipMovesScores :: State -> Move
zipMovesScores boardState = zip moves $ (map getScore) . (map (\ m -> makeMove m boardState)) $ moves
  where
    getScore :: State -> Int
    getScore s = boardEvaluationColor $ turn boardState

    moves :: [Move]
    moves = stateMoves boardState

