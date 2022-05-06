module RemnantSpec where

import Test.Hspec

import Cuboid (Source(..), Target(..))
import Remnant (Remnant, moveWhatsNotSourceIntoTheRemnant)
import Segment ( AxisResult(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , compareSegments
               )

emptyRemnant :: Remnant
emptyRemnant = []

target :: Target
target = Target [TrgSeg (30, 40), TrgSeg (30, 40), TrgSeg (30, 40)]

target2 = Target [TrgSeg (15, 17), TrgSeg (15, 17), TrgSeg (15, 17)]

source :: Source
source = Source [SrcSeg (10, 20), SrcSeg (10, 20), SrcSeg (10, 20) ]

srcSeg40 = SrcSeg (-40, 40)
srcSeg18 = SrcSeg (-18, 18)
srcSeg50 = SrcSeg (-50, 50)
trgSeg10 = TrgSeg (-10, 10)
trgSegIntersects = TrgSeg (-15, 25)
trgSeg30 = TrgSeg (-30, 30)

remnantSpec =
  describe "moveWhatsNotSourceIntoTheRemnant" $ do
    it "source and target don't overlap" $ do
      moveWhatsNotSourceIntoTheRemnant
          emptyRemnant
          source
          target
        `shouldBe`
          [target]
    it "source engulfs target completely" $ do
      moveWhatsNotSourceIntoTheRemnant
          emptyRemnant
          source
          target2
        `shouldBe`
          []
    context "Test compare where source and target intersect to generate adjacents" $ do
      it "the number of adjacent targets" $ do
        length (moveWhatsNotSourceIntoTheRemnant emptyRemnant (Source [srcSeg40, srcSeg18, srcSeg50]) (Target [trgSeg10, trgSegIntersects, trgSeg30]))
          `shouldBe`
            1
