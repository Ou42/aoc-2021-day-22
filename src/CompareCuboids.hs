module CompareCuboids where

{- | Gather all the information about the comparison
   | of two cuboids into this module and provide
   | functions that provide useful information about
   | the comparison.
-}
import Cuboid (Source(..), Target(..))
import Segment ( ResultType(..)
               , Segment(..)
               , SrcSeg(..)
               , TrgSeg(..)
               , AdjLeft(..)
               , AdjRight(..)
               , compareSegments
               )

type Compares = [Compare]

type Compare = (Axis, ResultType)

data Axis
   = X
   | Y
   | Z
   deriving (Eq, Ord, Show)

mkCompareCuboids :: Source -> Target -> Compares
mkCompareCuboids source target =
  let
      xr = (X, compareSegments (xSrc source) (x target))
      yr = (Y, compareSegments (ySrc source) (y target))
      zr = (Z, compareSegments (zSrc source) (z target))
  in
      [xr, yr, zr ]

{- A predicate for a desired comparison result
-}
resultType :: ResultType -> Compare -> Bool
resultType desiredResultType compare =
   desiredResultType == snd compare

{- A predicate for a segment comparison for a specific axis
-}
forAxis :: Axis -> Compare -> Bool
forAxis desiredAxis compare =
   desiredAxis == fst compare

