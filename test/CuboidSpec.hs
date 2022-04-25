module CuboidSpec where

import Test.Hspec

import Cuboid (Target(..), volume)
import Segment (Segment(..), TrgSeg(..))

cuboidSpec =
  describe "Testing Cuboids" $ do
    it "volumne" $ do
        volume (Target [TrgSeg (5, 8), TrgSeg (5, 9), TrgSeg (5, 10)])
          `shouldBe`
            (4 * 5 * 6)
