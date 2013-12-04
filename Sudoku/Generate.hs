module Sudoku.Generate where

import Control.Monad.State
import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as Map
import Sudoku.Base
import Sudoku.Solver.Backtracking
import System.Random

type RandState = State StdGen

permute :: Ord a => [a] -> RandState [a]
permute as = do
	bs <- replicateM (length as) (state random) :: RandState [Int]
	return . map snd . sort $ zip bs as

generate :: Solver -> RandState Board
generate solver = do
	constraints <- fmap Map.fromList $ forM positions $ \pos -> do
		vs <- permute [1..9]
		return (pos,vs)
	let answer = head $ backtrack (constraints !) (Board Map.empty)
	rpos <- permute positions
	return $ prune answer rpos
	where
		prune board [] = board
		prune board (p:ps) = if solves solver newBoard
			then prune newBoard ps
			else prune board ps
			where
				newBoard = Board . Map.delete p . getMap $ board

solves :: Solver -> Board -> Bool
solves solver board = case solver board of
	[x] -> isComplete x
	_ -> False
