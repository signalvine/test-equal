-- | This module provides 'Equal' instances for some standard types.
--
-- If you wish to override any of the provided instances, do not import
-- this module.
--
-- The instances provided by this module are:
--
-- * @instance ('Show' a, 'Equal' a) => 'Equal' [a]@
--
-- * A default overlappable @instance ('Eq' a, 'Show' a) => 'Equal' a where 'cmp' = 'cmpAtom'@
{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Equal.Instances () where

import Test.Equal
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Monoid ((<>))

instance (Show a, Equal a) => Equal [a] where
  cmp = cmpLabeledContainers $ \l1 l2 ->
    ( zip3 (map decimal [0::Int ..]) l1 l2
    , zip  (map decimal [length l2 ..]) (drop (length l2) l1)
    , zip  (map decimal [length l1 ..]) (drop (length l1) l2)
    )

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => Equal a where cmp = cmpAtom
