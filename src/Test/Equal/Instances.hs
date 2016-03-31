-- | This module provides 'Equal' instances for some standard types.
--
-- If you wish to override any of the provided instances, do not import
-- this module.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Equal.Instances (Equal) where

import Test.Equal
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Monoid ((<>))

instance (Show a, Equal a) => Equal [a] where
  cmp l1 l2 =
    let
      diffs1 = concat $ zipWith3
        (\i x1 x2 ->
          case cmp x1 x2 of
            Equal -> []
            NotEqual diff ->
              "Wrong element (!! " <> decimal i <> ")" : indent (ppDiffL diff)
        )
        [0::Int ..] l1 l2
      diffs2 = zipWith
        (\i x1 -> "Extra element (!! " <> decimal i <> "): " <> fromString (show x1))
        [length l2 ..] (drop (length l2) l1)
      diffs3 = zipWith
        (\i x2 -> "Missing element (!! " <> decimal i <> "): " <> fromString (show x2))
        [length l1 ..] (drop (length l1) l2)
      diffs = concat [diffs1, diffs2, diffs3]
    in
      if null diffs
        then Equal
        else NotEqual $ CustomDiff diffs
