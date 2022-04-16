{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq, Ord) -- fst <= snd

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
  | OverlapsTargetLeft

    {- | OverlapsTargetRight
       |
       |     ----- target -----/////////
       |                       -------------- source ------
    -}
  | OverlapsTargetRight

    {- | OverlapsTarget
       |
       |          /////// target ///////
       |     -------------- source -----------------
    -}
  | OverlapsTarget

    {- | EncompassedByTarget
       |
       |     -----//////// target //////-----------
       |          ------- source -------
    -}
  | EncompassedByTarget

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

