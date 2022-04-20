module RemnantSpec where

import Test.Hspec

import Cuboid (Source(..), Target(..))
import Remnant (Remnant, reduce)
import Segment ( ResultType(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , TargetAdjacentLeft(..)
               , TargetAdjacentRight(..)
               , compareSegments
               )

{- | Cache creating an empty remnant so that fewer lines
   | of code have to be written
-}
result :: Source -> Target -> Remnant
result = reduce []

target :: Target
target = Target { x = TrgSeg (30, 40), y = TrgSeg (30, 40), z = TrgSeg (30, 40) }

remnantSpec =
  describe "reduce" $ do
    it "source and target don't overlap" $ do
      result
          (Source { xSrc = SrcSeg (10, 20), ySrc = SrcSeg (10, 20), zSrc = SrcSeg (10, 20) })
          target
        `shouldBe`
          [target]

