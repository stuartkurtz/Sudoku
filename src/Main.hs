module Main where

import Control.Monad.State
import Sudoku.Base
import Sudoku.Generate
import Sudoku.Solver.Backtracking
import Sudoku.Solver.Logic
import System.Environment
import System.Random

runSolver :: Solver -> Board -> String
runSolver solver board = case solver board of
    [x] -> show x
    []  -> "no solution"
    _   -> "multiple solutions"

gen :: Solver -> IO ()
gen solver = do
    g <- getStdGen
    putStr . show $ evalState (generate solver) g

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
            board <- fmap read (readFile arg) :: IO Board
            putStr . unlines $
                [ "problem:\n"
                , indent 3 (show board)
                , "\nsolution:\n"
                , indent 3 (runSolver solver board)
                ]


indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

main :: IO()
main = do
    args <- getArgs
    if null args
        then gen lsolve
        else evalStateT (mapM_ processArg args) lsolve
