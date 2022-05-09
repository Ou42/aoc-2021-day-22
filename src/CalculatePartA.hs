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

    -- ISSUE: This is nuts!
    -- 1. At best, this is a cutesy hack that should be
    --    moved to be a mondule-direct function.
    -- 2. But that begs the question of why we have to do this
    --    hack anyways.
    -- 3. Plus, the realization that this is duplicated in Remnant
    --    is a smell.
    -- So, this needs to be addressed.
    noOverlap   = (Nothing, [])
  in
    if noOverlap `elem` axisResults then
      outgoingRemnant
    else
      let
         (outgoingTarget, _) = foldl sourceTargetIntersection (target, 0) axisResults
      in
      outgoingTarget : outgoingRemnant

{- | generate the overlap cuboid from the compare -}
sourceTargetIntersection :: (Target, Int) -> AxisResult -> (Target, Int)
sourceTargetIntersection (target, axisOffset) axisResult@(Just overlap, _) =
   createCommon overlap
   where
      createCommon :: TrgSeg -> (Target, Int)
      createCommon overlap' =
         ( createPiece overlap'
         , axisOffset + 1
         )

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
