{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq, Ord) -- fst <= snd

{- How arguments to compareSegments are passed
-}

newtype Source = Source (Int, Int) deriving (Eq, Ord, Show) -- fst <= snd
newtype Target = Target (Int, Int) deriving (Eq, Ord, Show) -- fst <= snd

{- Roles played by Segments returned by ResultType
-}
-- The adjacent segment to the left of the source segment.
newtype TargetAdjacentLeft  = TargetAdjacentLeft  Target deriving (Eq, Ord, Show)
 -- The adjacent segment to the right of the source segment.
newtype TargetAdjacentRight = TargetAdjacentRight Target deriving (Eq, Ord, Show)

{- | The catagories of results from combining two Segments together
   |
-}
data ResultType
  {- | The two segments do not intersect at all
  -}
  = NoOverlap

  {- | OverlapsTargetLeft
     |
     |                   ///////----- target ------
     |     ---- source ---------
  -}
  | OverlapsTargetLeft TargetAdjacentRight

  {- | OverlapsTargetRight
     |
     |     ----- target -----/////////
     |                       -------------- source ------
  -}
  | OverlapsTargetRight TargetAdjacentLeft

  {- | OverlapsTarget
     |
     |          /////// target ///////
     |     -------------- source -----------------
  -}
  | OverlapsTarget -- Doesn't return anything

  {- | OverlappedByTarget
     |
     |     -----//////// target //////-----------
     |          ------- source -------
  -}
  | OverlappedByTarget TargetAdjacentLeft TargetAdjacentRight -- targetAdjacentLeft targetAdjacentRight

  deriving (Eq, Ord, Show)

{- Important axiom: a segment's slope must not be negative
-}
compareSegments :: Source -> Target -> ResultType
compareSegments (Source(s1, s2)) (Target (t1, t2))
  | s2 < t1 = NoOverlap
  | s1 <= t1 && s2 >= t1 && s2 <= t2 = OverlapsTargetLeft (TargetAdjacentRight $ Target (s2 + 1, t2))
  | otherwise = undefined

{- | Rendering -}

instance Show Segment where
   show (Segment s) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"

{- | Parsing -}

{- "-36..17" -}
toSegment :: T.Text -> Segment
toSegment pairStr = Segment $ toTuple $ map readInt $ T.splitOn ".." pairStr

