module SegmentSpec where

import Test.Hspec

import Segment ( ResultType(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , TargetAdjacentLeft(..)
               , TargetAdjacentRight(..)
               , compareSegments
               )

segmentSpec =
  describe "compareSegments" $ do
    it "detects no overlap" $ do
      compareSegments (SrcSeg (5,6))  (TrgSeg (7,8)) `shouldBe` NoOverlap
    it "detects target is right-adjacent to SrcSeg" $ do
      compareSegments (SrcSeg (5,10)) (TrgSeg (8,15))
        `shouldBe`
          OverlapsTargetLeft (TargetAdjacentRight (TrgSeg (11, 15)))
    it "detects target is left-adjacent to SrcSeg" $ do
      compareSegments (SrcSeg (8, 15)) (TrgSeg (5, 10))
        `shouldBe`
          OverlapsTargetRight (TargetAdjacentLeft (TrgSeg (5, 7)))
    it "detects SrcSeg completely overlaps target" $ do
      compareSegments (SrcSeg (5, 15)) (TrgSeg (8, 10))
        `shouldBe`
          OverlapsTarget
    it "detects TrgSeg completely overlaps SrcSeg" $ do
      compareSegments (SrcSeg (8, 10)) (TrgSeg (5, 15))
        `shouldBe`
          OverlappedByTarget (TargetAdjacentLeft (TrgSeg (5, 7))) (TargetAdjacentRight (TrgSeg (11,15)))
