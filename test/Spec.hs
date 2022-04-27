module Main where

import Test.Hspec

import CuboidSpec (cuboidSpec)
import RebootStepSpec (rebootStepSpec)
import RemnantSpec (remnantSpec)
import SegmentSpec (segmentSpec)
import SegmentTestSpec (segmentTestSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Segment" $ do segmentSpec
  describe "Cuboid" $ do cuboidSpec
  -- describe "SegmentTest" $ do segmentTestSpec
  describe "RebootStep" $ do rebootStepSpec
  describe "Remnant" $ do remnantSpec
