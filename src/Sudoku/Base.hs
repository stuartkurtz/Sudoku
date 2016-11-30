-- | Base module for the Sudoku system. Defines common types, values. -}

module Sudoku.Base where

import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

-- | The type of a location on a Sudoku board.

type Position = (Int,Int)

-- | The list of valid `Position` values associated with a Sudoku board.

positions :: [Position]
positions = (,) <$> [1..9] <*> [1..9]

-- | The type of a Sudoku board.

newtype Board = Board { getMap :: Map Position Int }

instance Read Board where
    readsPrec _ = readP_to_S parseBoard where
        parseBoard = do
            entries <- replicateM 81 (skipSpaces >> satisfy isEntry)
            let assocs = filter (isDigit . snd) $ zip positions entries
            return . Board
                   . Map.fromList
                   . map (\(p,v) -> (p,ord v - ord '0'))
                   $ assocs
        isEntry c = isDigit c || c `elem` ".-_"


instance Show Board where
    show (Board board) = concatMap fmt positions where
        fmt pos = entry pos ++ separator pos
        separator (row,col)
            | rem col 3 /= 0 = " "
            | col < 9        = "   "
            | rem row 3 /= 0 = "\n"
            | row < 9        = "\n\n"
            | otherwise      = "\n"
        entry pos = case Map.lookup pos board of
            Just x  -> show x
            Nothing -> "."

isComplete :: Board -> Bool
isComplete (Board b) = Map.size b == 81

-- | The type of a Sudoku solver

type Solver = Board -> [Board]

