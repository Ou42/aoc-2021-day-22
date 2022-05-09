module SegmentSpec where

import Test.Hspec

import Segment ( AxisResult(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , compareSegments
               , dimension
               )

segmentSpec =
  describe "Testing segments" $ do
    describe "segment dimension" $ do
      it "positive to positive" $ do
        dimension (TrgSeg (5, 8))
          `shouldBe`
            4
      it "positive to negative" $ do
        dimension (TrgSeg (2, -2))
          `shouldBe`
            3 -- Seems like it should be 5, but we have no positive to negative.
      it "negative to negative" $ do
        dimension (TrgSeg (-22, -16))
          `shouldBe`
            7
    describe "compareSegments" $ do
      it "detects no overlap" $ do
        compareSegments (SrcSeg (5,6))  (TrgSeg (7,8)) `shouldBe` NoOverlap
      it "detects target is right-adjacent to SrcSeg" $ do
        compareSegments (SrcSeg (5,10)) (TrgSeg (8,15))
          `shouldBe`
            Intersects
              (Just $ TrgSeg (8, 10))
              [ TrgSeg (11, 15) ]
      it "detects target is left-adjacent to SrcSeg" $ do
        compareSegments (SrcSeg (8, 15)) (TrgSeg (5, 10))
          `shouldBe`
            Intersects
              ( Just $ TrgSeg (8, 10))
              [ TrgSeg (5, 7) ]
      it "detects SrcSeg completely overlaps target" $ do
        compareSegments (SrcSeg (5, 15)) (TrgSeg (8, 10))
          `shouldBe`
            TargetSwallowedBySource
      it "detects TrgSeg completely overlaps SrcSeg" $ do
        compareSegments (SrcSeg (8, 10)) (TrgSeg (5, 15))
          `shouldBe`
            Intersects
              ( Just $ TrgSeg (8, 10))
              [ TrgSeg (5, 7)
              , TrgSeg (11, 15)
              ]
