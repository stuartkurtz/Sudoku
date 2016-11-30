module Control.Union where

import Data.Set (Set,empty,union)

-- | The `Union` type is simply `Set` as a `Monoid` via `union`.

newtype Union a = Union { getUnion :: Set a }

instance Ord a => Monoid (Union a) where
    mempty = Union empty
    mappend x y = Union (getUnion x `union` getUnion y)

