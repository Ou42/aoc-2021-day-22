module CompareCuboidsSpec where

import Test.Hspec

import CompareCuboids ( Axis(..)
                      , Compare
                      , Compares
                      , forAxis
                      , mkCompareCuboids
                      , resultType
                      )
import Cuboid (Source(..), Target(..))
import Segment ( ResultType(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , TargetAdjacentLeft(..)
               , TargetAdjacentRight(..)
               , compareSegments
               )

compareCuboidsSpec =
  describe "compare cuboids" $ do
    it "something should happen" $ do
      5 `shouldBe` 5
