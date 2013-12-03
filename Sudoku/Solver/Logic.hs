-- | A Sudoku solver based on logic.

module Sudoku.Solver.Logic where

import Control.Monad.Writer
import Control.Monad.State
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set,(\\),empty,intersection,union,unions)
import qualified Data.Set as Set
import Sudoku.Base

-- | A variable is given the same type as a `Board` association for computational
--   convenience.

type Var = (Position,Int)

-- | The variable associated with a given row, column, and value.

mkVar :: Int -- ^ row
	  -> Int -- ^ column
	  -> Int -- ^ value
	  -> Var
mkVar row col val = ((row,col),val)

-- | The type of a 1-clause in our logic. A 1-clause asserts that precisely one
--   of a set of variables is true.

type OneClause = Set Var

-- | The type of a theory in our logic.

type Theory = [OneClause]

-- | The initial `Theory`, which consists the list of 1-clauses that makes up
--   the constraints of Sudoku.

initialTheory :: Theory
initialTheory = concatMap genTheory [rvt,rct,cvt,bvt] where
    rvt row val idx = mkVar row idx val
    rct row col idx = mkVar row col idx
    cvt col val idx = mkVar idx col val
    bvt blk val idx = mkVar (3*mjr+mnr+1) (3*mjc+mnc+1) val where
    	(mjr,mjc) = divMod (blk-1) 3
    	(mnr,mnc) = divMod (idx-1) 3
    genTheory f = [Set.fromList [f a b idx | idx <- [1..9]] | a <- [1..9],b <- [1..9]]

-- | The `Union` type is simply `Set` as a `Monoid` via `union`.
 
newtype Union a = Union { getSet :: Set a }

instance Ord a => Monoid (Union a) where
	mempty = Union empty
	mappend x y = Union (getSet x `union` getSet y)

-- | The reduction monad. A computation that reduces the `Theory` contained in the
--   `State`, while writing a set of variables that become asserted through the
--   reduction process.

type Reduction = StateT Theory (Writer (Union Var))

-- | Construct a `Reduction` that begins with asserting the argument's variables.

reduceAsserts :: Set Var -> Reduction ()
reduceAsserts vars = do
	tell $ Union vars
	theory <- get
	let (reducedTheory,denials) = eitherMap reduceClause theory
	put reducedTheory
	unless (null denials) $ reduceDenials (unions denials)
	where
		reduceClause clause = case Set.size (vars `intersection` clause) of
			0 -> Left clause
			1 -> Right (clause \\ vars)
			_ -> error "inconsistent problem"

-- | Construct a `Reduction` that begins with denying the argument's variables.

reduceDenials :: Set Var -> Reduction ()
reduceDenials denials = do
	theory <- get
	let (reducedTheory,asserts) = eitherMap reduceClause theory
	put reducedTheory
	unless (null asserts) $ reduceAsserts (unions asserts)
	where
		reduceClause clause = case Set.size diffs of
			0 -> error "inconsistent problem"
			1 -> Right diffs
			_ -> Left diffs
			where
				diffs = clause \\ denials

-- | A logic based Sudoku solver. Note that this will return one solution, which may
--   not be complete.

lsolve :: Solver
lsolve board = [mkBoard . execReduction $ reduceAsserts vars]
	where
		vars = Set.fromList . Map.toList . getMap $ board
		execReduction r = execWriter . execStateT r $ initialTheory
		mkBoard = Board . Map.fromList . Set.toList . getSet

-- | A mashup of `map` and `partitionEithers`.

eitherMap :: (a -> Either b c) -> [a] -> ([b],[c])
eitherMap f = partitionEithers . map f

