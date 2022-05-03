module Main where

import Test.Hspec

import CuboidSpec (cuboidSpec)
import RebootStepSpec (rebootStepSpec)
import RemnantSpec (remnantSpec)
import SegmentSpec (segmentSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Segment" $ do segmentSpec
  describe "Cuboid" $ do cuboidSpec
  describe "RebootStep" $ do rebootStepSpec
  describe "Remnant" $ do remnantSpec
