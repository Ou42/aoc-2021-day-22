{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq, Ord) -- fst <= snd

{- How arguments to compareSegments are passed
-}

newtype SrcSeg = SrcSeg (Int, Int) deriving (Eq, Ord) -- fst <= snd
newtype TrgSeg = TrgSeg (Int, Int) deriving (Eq, Ord, Show) -- fst <= snd

{- | Roles played by Segments returned by AxisResult
-}
-- | The adjacent segment to the left of the source segment.
newtype AdjLeft  = AdjLeft  TrgSeg deriving (Eq, Ord, Show)
-- | The adjacent segment to the right of the source segment.
newtype AdjRight = AdjRight TrgSeg deriving (Eq, Ord, Show)

-- | The segment that defines the overlap over the source and target segments
newtype Overlap = Overlap TrgSeg deriving (Eq, Ord, Show)

data AxisResult = (Maybe TrgSeg, [TrgSeg])

{- | The catagories of results from combining two Segments together
   |
   | We're calling it an AxisResult because a segment-pair comprises the dimension
   | of one of the cuboid's sides.
-}
data AxisResult'
  {- | The two segments do not intersect at all
  -}
  = NoOverlap

  {- | OverlapsLeft
     |
     |                   ///////----- target ------
     |     ---- source ---------
  -}
  | OverlapsLeft Overlap AdjRight -- is *not* the return result that Remnant is going to get!

  {- | OverlapsRight
     |
     |     ----- target -----/////////
     |                       -------------- source ------
  -}
  | OverlapsRight AdjLeft Overlap

  {- | Overlaps
     |
     |          /////// target ///////
     |     -------------- source -----------------
  -}
  | Overlaps -- Doesn't return anything

  {- | OverlappedByTarget
     |
     |     -----//////// target //////-----------
     |          ------- source -------
  -}
  | OverlappedByTarget AdjLeft Overlap AdjRight

  deriving (Eq, Ord, Show)

{- Important axiom: a segment's slope must not be negative
-}
compareSegments :: SrcSeg -> TrgSeg -> AxisResult
compareSegments (SrcSeg (s1, s2)) (TrgSeg (t1, t2))
  | s2 < t1 || t2 < s1 =                              -- no overlap
      (Nothing, [])
  | s1 <= t1 && s2 >= t1 && s2 <= t2 =                -- overlaps on the left
      ( Just TrgSeg (t1, s2)                          --                   ///////----- AdjRight ------
      , [ TrgSeg (s2 + 1, t2) ]                       --     ---- source ---------
      )
  | s1 > t1 && s2 >= t2 =                             -- overlaps right
      ( Just TrgSeg (s1, t2)                          --         ----- AdjLeft ---///////
      , [ TrgSeg (t1, s1 - 1) ]                       --                          ---------- source -------
      )
  | s1 <= t1 && s2 >= t1 =                            -- source overlaps target
      ( Just TrgSeg (t1, t2)
      , []
      )
  | t1 < s1 && s2 < t2 =                              -- overlapped by target
      ( Just TrgSeg (s1, s2)
      , [ TrgSeg (t1, s1 - 1)
        , TrgSeg (s2 + 1, t2)
        ]
      )
  | otherwise = undefined

{-| Convert SrcSeg to TrgSeg
-}
convert :: SrcSeg -> TrgSeg
convert (SrcSeg seg) =
  TrgSeg seg

{- | Rendering -}

instance Show SrcSeg where
   show (SrcSeg s) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"

{- | Parsing -}

{- "-36..17" -}
toSrcSeg :: T.Text -> SrcSeg
toSrcSeg pairStr = SrcSeg $ toTuple $ map readInt $ T.splitOn ".." pairStr

dimension :: TrgSeg -> Int
dimension (TrgSeg (start, end)) =
   abs (end + 1 - start)
