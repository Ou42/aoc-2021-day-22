module CompareCuboidsSpec where

import Test.Hspec

import CompareCuboids ( AxisResults
                      , mkAxisResults
                      )
import Cuboid (Source(..), Target(..))
import Segment ( AxisResult(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , AdjLeft(..)
               , AdjRight(..)
               , compareSegments
               )

compareCuboidsSpec =
  describe "compare cuboids" $ do
    it "something should happen" $ do
      5 `shouldBe` 5
