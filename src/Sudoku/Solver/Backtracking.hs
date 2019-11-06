-- | A backtracking Sudoku solver.

module Sudoku.Solver.Backtracking where

import Control.Monad.NState
import Data.List ((\\),delete,nub,intersect)
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Maybe
import Sudoku.Base

-- | A backtracking Sudoku solver.

bsolve :: Solver
bsolve = backtrack (const [1..order^two])

-- | A backtracking solver with additional positional constraints.

backtrack :: (Position -> [Int]) -> Solver
backtrack constraints startBoard
    = execNState (traverse extend positions) startBoard
    where
    candidates pos = case Map.lookup pos (getMap startBoard) of
        Just x -> [x] `intersect` constraints pos
        Nothing -> constraints pos
    extend pos = do
        board <- getMap <$> get
        val <- branch (candidates pos \\ range board (neighbors ! pos))
        put . Board $ Map.insert pos val board

-- | A 'Map' from positions on the Sudoku board to the list of
--   conflicting positions.

neighbors :: Map Position [Position]
neighbors =  Map.unions [ Map.singleton p (neighborf p)
                        | p <- positions
                        ]
    where
    neighborf pos =
        delete pos . nub . concatMap ($ pos) $ [rowOf,colOf,blockOf]
    rowOf (row,_) = map ((,) row)  [1..order^two]
    colOf (_,col) = map (flip (,) col) [1..order^two]
    blockOf (row,col) =
        [ (trunc row + rx,trunc col + cx)
        | rx <- [1..order]
        , cx <- [1..order]
        ]
    trunc x = nx - nx `mod` order where nx = x-1

-- | The list of values from a map associated with a list of keys.
--   It is not an error for a key to be missing from a map, such
--   keys simply contribute nothing to the result.

range :: Ord a => Map a b -> [a] -> [b]
range m = mapMaybe (`Map.lookup` m)
