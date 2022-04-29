module CalculatePartA where

import Cuboid (volume, Source(..), Target(..))
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant, emptyRemnant, mkAxisResults)
import qualified RunningCode as RC
import Segment (AdjLeft(..), AdjRight(..), AxisResult(..), Overlap(..), SrcSeg(..), TrgSeg(..))

calculatePartA :: Remnant -> Remnant
calculatePartA = foldl truncateTarget emptyRemnant

truncateTarget :: Remnant -> Target -> Remnant
truncateTarget outgoingRemnant target =
  let
    axisResults = mkAxisResults region50Source target
  in
    if NoOverlap `elem` axisResults then
      outgoingRemnant
    else
      let
         (outgoingTarget, _) = foldl sourceTargetIntersection (target, 0) axisResults
      in
      outgoingTarget : outgoingRemnant

{- | generate the overlap cuboid from the compare -}
sourceTargetIntersection :: (Target, Int) -> AxisResult -> (Target, Int)
sourceTargetIntersection (target, axisOffset) axisResult =
   case axisResult of
      Overlaps ->
         (target, axisOffset + 1)
      OverlapsLeft (Overlap overlap) (AdjRight _) ->
         createCommon overlap
      OverlapsRight (AdjLeft _) (Overlap overlap) ->
         createCommon overlap
      OverlappedByTarget (AdjLeft _) (Overlap overlap) (AdjRight _) ->
         createCommon overlap
   where
      createCommon :: TrgSeg -> (Target, Int)
      createCommon overlap' =
         ( createPiece overlap'
         , axisOffset + 1
         )

      {- | Return a torn-off piece of the original cuboid -}
      createPiece :: TrgSeg -> Target
      createPiece segment =
        let
          Target incoming = target
        in
        Target $ take axisOffset incoming <> [segment] <> drop (axisOffset+1) incoming

region50 :: SrcSeg
region50 = SrcSeg (-50, 50)

region50Source = Source [region50, region50, region50]
