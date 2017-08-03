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
import Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

instance (Show a, Equal a) => Equal [a] where
  cmp = cmpLabeledContainers $ \l1 l2 ->
    ( zip (map decimal [0::Int ..]) (zip l1 l2)
    , zip (map decimal [length l2 ..]) (drop (length l2) l1)
    , zip (map decimal [length l1 ..]) (drop (length l1) l2)
    )

instance (Show a, Show b, Ord a, Equal b) => Equal (Map.Map a b) where
  cmp = cmpLabeledContainers $ \l1 l2 ->
    ( map (first (fromString . show)) . Map.toList $ Map.intersectionWith (,) l1 l2
    , map (first (fromString . show)) . Map.toList $ Map.difference l1 l2
    , map (first (fromString . show)) . Map.toList $ Map.difference l2 l1
    )

instance (Show b, Equal b) => Equal (IntMap.IntMap b) where
  cmp = cmpLabeledContainers $ \l1 l2 ->
    ( map (first decimal) . IntMap.toList $ IntMap.intersectionWith (,) l1 l2
    , map (first decimal) . IntMap.toList $ IntMap.difference l1 l2
    , map (first decimal) . IntMap.toList $ IntMap.difference l2 l1
    )

instance (Show a, Show b, Eq a, Hashable a, Equal b) => Equal (HM.HashMap a b) where
  cmp = cmpLabeledContainers $ \l1 l2 ->
    ( map (first (fromString . show)) . HM.toList $ HM.intersectionWith (,) l1 l2
    , map (first (fromString . show)) . HM.toList $ HM.difference l1 l2
    , map (first (fromString . show)) . HM.toList $ HM.difference l2 l1
    )

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => Equal a where cmp = cmpAtom
