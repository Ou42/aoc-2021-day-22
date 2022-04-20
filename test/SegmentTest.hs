module SegmentTest where

{- | Testing-assist functions for SegmentSpec
-}
import Segment (Segment(..))

{- | AdjacentRight
   |
   |                          ----- target ------
   |     ---- source ---------
-}
targetIsAdjacentRight :: Segment -> Segment -> Bool
targetIsAdjacentRight src trgt = undefined

{- | AdjacentLeft
   |
   |     ----- target ------
   |                        ---- source ---------
-}
targetIsAdjacentLeft :: Segment -> Segment -> Bool
targetIsAdjacentLeft src trgt = undefined

    {- | Same
       |
       |          //////// target //////
       |          ------- source -------
    -}
areSame :: Segment -> Segment -> Bool
areSame src trgt = undefined
