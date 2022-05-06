module CalculatePartA where

import Cuboid (volume, Source(..), Target(..))
import RebootStep (generateRemnant, parseInputText)
import Remnant (Remnant, emptyRemnant, mkAxisResults)
import qualified RunningCode as RC
import Segment (AxisResult(..), SrcSeg(..), TrgSeg(..))

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
      TargetSwallowedBySource ->
         (target, axisOffset + 1)
      Intersects (Just overlap) _ ->
         (createPiece overlap, axisOffset + 1)
   where

      {- | Return a torn-off piece of the original cuboid -}
      -- ISSUE: Duplicated in Remnant.  Also, why can't we call this
      -- something more accurate like 'createFragmentTarget'
      createPiece :: TrgSeg -> Target
      createPiece segment =
        let
          Target incoming = target
        in
        Target $ take axisOffset incoming <> [segment] <> drop (axisOffset+1) incoming

region50 :: SrcSeg
region50 = SrcSeg (-50, 50)

region50Source = Source [region50, region50, region50]
