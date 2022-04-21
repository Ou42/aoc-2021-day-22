{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq, Ord) -- fst <= snd

{- How arguments to compareSegments are passed
-}

newtype SrcSeg = SrcSeg (Int, Int) deriving (Eq, Ord) -- fst <= snd
newtype TrgSeg = TrgSeg (Int, Int) deriving (Eq, Ord, Show) -- fst <= snd

{- | Roles played by Segments returned by ResultType
-}
-- | The adjacent segment to the left of the source segment.
newtype TargetAdjacentLeft  = TargetAdjacentLeft  TrgSeg deriving (Eq, Ord, Show)
-- | The adjacent segment to the right of the source segment.
newtype TargetAdjacentRight = TargetAdjacentRight TrgSeg deriving (Eq, Ord, Show)

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
  | OverlappedByTarget TargetAdjacentLeft TargetAdjacentRight

  deriving (Eq, Ord, Show)

{- Important axiom: a segment's slope must not be negative
-}
compareSegments :: SrcSeg -> TrgSeg -> ResultType
compareSegments (SrcSeg(s1, s2)) (TrgSeg (t1, t2))
  | s2 < t1 =
      NoOverlap
  | s1 <= t1 && s2 >= t1 && s2 <= t2 =
      OverlapsTargetLeft (TargetAdjacentRight $ TrgSeg (s2 + 1, t2))
  | s1 > t1 && s2 >= t2 =
      OverlapsTargetRight (TargetAdjacentLeft $ TrgSeg (t1, s1 - 1))
  | s1 <= t1 && s2 >= t1 =
      OverlapsTarget
  | t1 < s1 && s2 < t2 =
      OverlappedByTarget (TargetAdjacentLeft $ TrgSeg (t1, s1 - 1)) (TargetAdjacentRight $ TrgSeg (s2 + 1, t2))
  | otherwise = undefined

{-| Convert SrcSeg to TrgSeg
-}
convert :: SrcSeg -> TrgSeg
convert (SrcSeg (start, end)) =
  TrgSeg (start, end)

{- | Rendering -}

instance Show SrcSeg where
   show (SrcSeg s) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"

{- | Parsing -}

{- "-36..17" -}
toSrcSeg :: T.Text -> SrcSeg
toSrcSeg pairStr = SrcSeg $ toTuple $ map readInt $ T.splitOn ".." pairStr

