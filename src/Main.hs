-- | The driver module for the Sudoku generator/solver.

module Main where

import Control.Monad.State
import Sudoku.Base
import Sudoku.Generate
import Sudoku.Solver.Backtracking
import Sudoku.Solver.Logic
import System.Environment
import System.Random

-- | A function that runs a 'Solver' on a 'Board', returning either
--   a solution or an appropriate message.

runSolver :: Solver -> Board -> String
runSolver solver board = case solver board of
    [x] -> show x
    []  -> "no solution"
    _   -> "multiple solutions"

-- | Given a solver, generate a Sudoku problem that can be solved
--   by the solver, and print the result to stdout.

gen :: Solver -> IO ()
gen solver = do
    g <- getStdGen
    putStr . show $ evalState (generate solver) g

-- | Generate a hard Sudoku instance, i.e., an instance which can
--   be solved by the backtracking solver, but not by the logic
--   based solver. Great for frustrating your friends!

hard :: IO ()
hard = do
    g <- getStdGen
    putStr . show $ evalState genHard g
    where
    genHard = do
        b <- generate bsolve
        if isComplete (head (lsolve b))
            then genHard
            else return b

-- | Process a command line argument. Note that arguments that don't
--   have the form of a known command-line flag are interpreted as the
--   path to a file containing a Sudoku problem to be solved using the
--   current solver.

processArg :: String -> StateT Solver IO ()
processArg arg
    | arg `elem` ["-h","--hard"] = liftIO hard
    | arg `elem` ["-l","--logic"] = put lsolve
    | arg `elem` ["-b","--backtrack"] = put bsolve
    | arg `elem` ["-g","--generate","--gen"] = do
        solver <- get
        liftIO $ gen solver
    | otherwise = do
        solver <- get
        liftIO $ do
            board <- read <$> readFile arg :: IO Board
            putStr . unlines $
                [ "problem:\n"
                , indent 3 (show board)
                , "\nsolution:\n"
                , indent 3 (runSolver solver board)
                ]

-- | Indent a block of text by given number of spaces.

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | The main act. With no arguments given, generate a Sudoku instance
--   using the logic-based solver.

main :: IO()
main = do
    args <- getArgs
    if null args
        then gen lsolve
        else evalStateT (void $ traverse processArg args) lsolve
