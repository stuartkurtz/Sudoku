-- | A module which provides a 'Union' wrapper for 'Data.Set' together
--   with the natural 'Monoid' instance.

module Data.Union where

import Data.Set (Set,empty,union)

-- | The `Union` type is simply `Set` as a `Monoid` via `union`.

newtype Union a = Union { getUnion :: Set a }

-- | The natural 'Semigroup' type class for 'Union'.

instance Ord a => Semigroup (Union a) where
    x <> y = Union (getUnion x `union` getUnion y)

-- | The natural 'Monoid' type class for 'Union'. 

instance Ord a => Monoid (Union a) where
    mempty = Union empty
