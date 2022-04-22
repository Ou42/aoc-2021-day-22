{- HLINT ignore -}

module SegmentTestSpec where

import Test.Hspec

import Segment (AxisResult(..), Segment(..), compareSegments)
import SegmentTest (areSame, targetIsAdjacentLeft, targetIsAdjacentRight)

segmentTestSpec =
  describe "Segment overlap predicates" $ do
    describe "areSame" $ do

      it "should verify segments are not same" $ do
        pending

      it "should verify segments are same" $ do
        pending

    describe "targetIsAdjacentLeft" $ do

      it "is true" $ do
        pending

      it "is false" $ do
        pending

    describe "targetIsAdjacentRight" $ do

      it "is true" $ do
        pending

      it "is false" $ do
        pending

