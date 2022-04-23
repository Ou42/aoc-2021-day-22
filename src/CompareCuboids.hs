module CompareCuboids where

{- | Gather all the information about the comparison
   | of two cuboids into this module and provide
   | functions that provide useful information about
   | the comparison.
-}
import Cuboid (Source(..), Target(..))
import Segment ( AxisResult(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , AdjLeft(..)
               , AdjRight(..)
               , compareSegments
               )

type AxisResults = [AxisResult]

mkAxisResults :: Source -> Target -> AxisResults
mkAxisResults (Source ss) (Target ts) = zipWith compareSegments ss ts

