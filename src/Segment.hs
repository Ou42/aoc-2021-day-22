{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq, Ord) -- fst <= snd

{- Make directly comparing a SrcSeg to a TrgSet impossible and vice-versa
-}
newtype SrcSeg = SrcSeg (Int, Int) deriving (Eq, Ord)       -- fst must be <= snd
newtype TrgSeg = TrgSeg (Int, Int) deriving (Eq, Ord, Show) -- fst must be <= snd

{- | The catagories of results from combining two Segments together
-}
data AxisResult
  = NoOverlap
  | Intersects (Maybe TrgSeg) [TrgSeg]
  | TargetSwallowedBySource
      deriving (Eq, Ord, Show)

{- | Axiom: a segment's slope must not be negative
-}
compareSegments :: SrcSeg -> TrgSeg -> AxisResult
compareSegments (SrcSeg (s1, s2)) (TrgSeg (t1, t2))
  | s2 < t1 || t2 < s1 =                     -- no overlap
      NoOverlap
  | s1 <= t1 && s2 >= t1 && s2 <= t2 =       -- overlaps on the left
      Intersects
        ( Just $ TrgSeg (t1, s2))            --                   ///////----- AdjRight ------
            [ TrgSeg (s2 + 1, t2) ]          --     ---- source ---------
  | s1 > t1 && s2 >= t2 =                    -- overlaps right
      Intersects
        ( Just $ TrgSeg (s1, t2))            --         ----- AdjLeft ---///////
            [ TrgSeg (t1, s1 - 1) ]          --                          ---------- source -------
  | s1 <= t1 && s2 >= t1 =                   -- source overlaps target
      TargetSwallowedBySource                --                /////// target ///////
                                             --       -------------- source  -----------------
  | t1 < s1 && s2 < t2 =                     -- overlapped by target
      Intersects
        ( Just $ TrgSeg (s1, s2))            --        -----//////// target //////-----------
            [ TrgSeg (t1, s1 - 1)            --             ------- source -------
            , TrgSeg (s2 + 1, t2)
            ]
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
