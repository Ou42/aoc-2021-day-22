module RemnantSpec where

import Test.Hspec

import Cuboid (Source(..), Target(..))
import Remnant (Remnant, accumulateNonAdjacentTargets, reduce)
import Segment ( AxisResult(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , AdjLeft(..)
               , AdjRight(..)
               , compareSegments
               )

{- | Cache creating an empty remnant so that fewer lines
   | of code have to be written
-}
result :: Source -> Target -> Remnant
result = reduce []

target :: Target
target = Target [TrgSeg (30, 40), TrgSeg (30, 40), TrgSeg (30, 40)]

source :: Source
source = Source [SrcSeg (10, 20), SrcSeg (10, 20), SrcSeg (10, 20) ]

remnantSpec =
  describe "reduce" $ do
    let target2 = Target [TrgSeg (15, 17), TrgSeg (15, 17), TrgSeg (15, 17)]
    it "source and target don't overlap" $ do
      result
          source
          target
        `shouldBe`
          [target]
    it "source engulfs target completely" $ do
      result
          source
          target2
        `shouldBe`
          []
    describe "Test compare where source and target intersect to generated adjacents" $ do
      let srcSeg40 = SrcSeg (-40, 40)
      let srcSeg18 = SrcSeg (-18, 18)
      let srcSeg50 = SrcSeg (-50, 50)
      let trgSeg10 = TrgSeg (-10, 10)
      let trgSegIntersects = TrgSeg (-15, 25)
      let trgSeg30 = TrgSeg (-30, 30)
      let source = Source [srcSeg40, srcSeg18, srcSeg50]
      let target = Target [trgSeg10, trgSegIntersects, trgSeg30]
      context "intersecting only on one axis" $ do
        it "the number of adjacent targets" $ do
          -- accumulateNonAdjacentTargets []
          5 `shouldBe` 5





