{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq, Ord) -- fst <= snd

{- Roles played by Segments returned by ResultType
-}
type TargetAdjacentLeft = Segment -- The adjacent segment to the left of the source segment.
type TargetAdjacentRight = Segment -- The adjacent segment to the right of the source segment.

{- | The catagories of results from combining two Segments together
   |
-}
data ResultType
  = NonIntersecting

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

compareSegments :: Segment -> Segment -> ResultType
compareSegments source target = undefined

augment :: Segment -> Segment -> [Segment]
augment s1 s2 =
  undefined

reduce :: Segment -> Segment -> [Segment]
reduce s1 s2 =
  undefined

{- | Rendering -}

instance Show Segment where
   show (Segment s) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"

{- | Parsing -}

{- "-36..17" -}
toSegment :: T.Text -> Segment
toSegment pairStr = Segment $ toTuple $ map readInt $ T.splitOn ".." pairStr

