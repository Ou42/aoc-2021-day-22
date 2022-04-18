module SegmentSpec where

import Test.Hspec

import Segment (ResultType(..), Segment(..), Source(..), Target(..), TargetAdjacentRight(..), compareSegments)

segmentSpec =
  describe "compareSegments" $ do
    it "detects no overlap" $ do
      compareSegments (Source (5,6))  (Target (7,8)) `shouldBe` NoOverlap
    it "detects target is right-adjacent to source" $ do
      compareSegments (Source (5,10)) (Target (8,15)) `shouldBe` OverlapsTargetLeft (TargetAdjacentRight (Target (11, 15)))

