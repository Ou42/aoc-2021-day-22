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

{- | The catagories of results from combining two Segments together
   |
   | We're calling it an AxisResult because a segment-pair comprises the dimension
   | of one of the cuboid's sides.
-}
-- newtype AxisResult = AxisResult (Maybe TrgSeg, [TrgSeg]) deriving (Eq, Ord, Show)
type AxisResult = (Maybe TrgSeg, [TrgSeg])

{- Important axiom: a segment's slope must not be negative
-}
-- ISSUE: We've left out one piece of information that
-- causes the code that uses this function to do too much
-- processing, including recalculating a target when it doesn't have to.
-- The problem is that we were counting on having the caller handle
-- all of the using the same approach.  That has turned out to not be
-- cleanly possible. Indeed the only responses here that can be handled
-- identically are the intersection responses; the 'no overlap' and the
-- 'source overlaps target' responses should be handled individully.
-- Make it cleanly top-down so that maintainers are not having to
-- work so hard wondering what's
-- going on.
-- Suggest reintroducing 3 sum types to break this up again:
-- 'OverLap', 'Intersects', and (I'll think of something better:)
-- 'TargetSwallowedBySource' .  The calling functions will thank us.
compareSegments :: SrcSeg -> TrgSeg -> AxisResult
compareSegments (SrcSeg (s1, s2)) (TrgSeg (t1, t2))
  | s2 < t1 || t2 < s1 =                 -- no overlap
      (Nothing, [])
  | s1 <= t1 && s2 >= t1 && s2 <= t2 =   -- overlaps on the left
      ( Just $ TrgSeg (t1, s2)           --                   ///////----- AdjRight ------
      , [ TrgSeg (s2 + 1, t2) ]          --     ---- source ---------
      )
  | s1 > t1 && s2 >= t2 =                -- overlaps right
      ( Just $ TrgSeg (s1, t2)           --         ----- AdjLeft ---///////
      , [ TrgSeg (t1, s1 - 1) ]          --                          ---------- source -------
      )
  | s1 <= t1 && s2 >= t1 =               -- source overlaps target
      ( Just $ TrgSeg (t1, t2)           --                /////// target ///////
      , []                               --           -------------- source -----------------
      )
  | t1 < s1 && s2 < t2 =                 -- overlapped by target
      ( Just $ TrgSeg (s1, s2)           --        -----//////// target //////-----------
      , [ TrgSeg (t1, s1 - 1)            --             ------- source -------
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
