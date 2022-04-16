{- HLINT ignore -}

module CuboidSpec where

import Test.Hspec

import Cuboid (Cuboid(..), nextSegment)
import Segment (Segment(..))

-- import SegmentTest (areSame, targetIsAdjacentLeft, targetIsAdjacentRight)

cuboidSpec =
  describe "Cuboid functionality" $ do
    let c = Cuboid { x = Segment (1, 2), y = Segment (3, 4), z = Segment (5, 6)  }
    context "next segment in cuboid" $ do
      it "returns y for x" $ do
        nextSegment (x c) c `shouldBe` (y c)
      it "returns z for y" $ do
        nextSegment (y c) c `shouldBe` (z c)
      it "returns y for x" $ do
        nextSegment (z c) c `shouldBe` (x c)
