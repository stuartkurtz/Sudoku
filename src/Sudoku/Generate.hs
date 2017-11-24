-- | Generate a Sudoku problem, given a Sudoku solver.

module Sudoku.Generate where

import Control.Monad.State
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Traversable (for)
import Sudoku.Base
import Sudoku.Solver.Backtracking
import System.Random

-- | The standard random 'State' type constructor.

type RandState = State StdGen

-- | Randomly permute a list, returning the result in the 'RandState'
--   context.

permute :: [a] -> RandState [a]
permute as = do
    bs <- replicateM (length as) (state random) :: RandState [Int]
    pure . map snd . sortBy (comparing fst) $ zip bs as

-- | Generate a Sudoku problem, given a solver.

generate :: Solver -> RandState Board
generate solver = do
    constraints <- Map.unions
                <$> ( for positions $ \pos -> do
                        Map.singleton pos <$> permute [1..9]
                    )
    let answer = head $ backtrack (constraints Map.!) (Board Map.empty)
    prune answer <$> permute positions
    where
        prune = foldl $ \board pos ->
            let newBoard = Board . Map.delete pos . getMap $ board
            in if solver `solves` newBoard
               then newBoard
               else board

-- | A binary predicate which holds when a solver generates a single,
--   complete solution to a Sudoku problem.

solves :: Solver -> Board -> Bool
solves solver board = case solver board of
    [x] -> isComplete x
    _ -> False
