module Main where

import Test.Hspec
import qualified Data.Text as T

import CompareCuboidsSpec (compareCuboidsSpec)
import RebootStepSpec (rebootStepSpec)
import RemnantSpec (remnantSpec)
import SegmentSpec (segmentSpec)
import SegmentTestSpec (segmentTestSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- describe "Cuboid" $ do cuboidSpec
  describe "Segment" $ do segmentSpec
  -- describe "SegmentTest" $ do segmentTestSpec
  describe "CompareCuboids" $ do compareCuboidsSpec
  describe "RebootStep" $ do rebootStepSpec
  describe "Remnant" $ do remnantSpec
