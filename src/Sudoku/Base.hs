-- | Base module for the Sudoku system. Defines common types, values. -}

module Sudoku.Base where

import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Text.Printf

-- | The order of the Sudoku system

order :: Int
order = 4

two, four :: Int
two = 2
four = 4

-- | The type of a location on a Sudoku board.

type Position = (Int,Int)

-- | The list of valid `Position` values associated with a Sudoku board.

positions :: [Position]
positions = (,) <$> [1..order^two] <*> [1..order^two]

-- | The type of a Sudoku board.

newtype Board = Board { getMap :: Map Position Int }

-- | A simple read instance for a 'Board', based on the natural file
--   format.

instance Read Board where
    readsPrec _ = readP_to_S parseBoard where
        parseBoard = do
            entries <- replicateM (order^four) (skipSpaces >> getEntry)
            let assocs = filter (all isDigit . snd) $ zip positions entries
            pure . Board
                 . foldMap (\(p,v) -> Map.singleton p (read v))
                 $ assocs
        getEntry = munch1 isDigit <++ string "." <++ string "-" <++ string "_"

-- | A simple 'Show' instance for 'Board', compatible with its 'Read'
--   instance.

instance Show Board where
    show (Board board) = concatMap fmt positions where
        fmt pos = entry pos ++ separator pos
        separator (row,col)
            | col `rem` order /= 0 = " "
            | col < order^two      = "   "
            | row `rem` order /= 0 = "\n"
            | row < order^two      = "\n\n"
            | otherwise            = "\n"
        entry pos = case Map.lookup pos board of
            Just x  -> printf "%2d" x
            Nothing -> " ."

-- | A predicate on 'Board' which holds when the board contains a value
--   for every position. Note that this implementation is intended for
--   speed, and could be mislead by bad code.

isComplete :: Board -> Bool
isComplete (Board b) = Map.size b == order^four

-- | The type of a Sudoku solver

type Solver = Board -> [Board]

