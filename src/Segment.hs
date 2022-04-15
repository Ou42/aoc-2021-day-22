{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq) -- fst <= snd

{- | The catagories of results from combining two Segments together
   |
-}
data ResultType
  = NotIntersecting
  | Adjacent
  | Skinned Int -- They are combined in single cube vector (the skin)
  | Intersects Segment
  | Encompasses [Segment] -- 2 segments, 1st is left difference; 2nd right difference.  Lined up edges are an issue.
  | EncompassedBy [Segment] -- Identical to previous; segments are reverse order
  | Same     -- Segments are identical

instance Show Segment where
   show (Segment s) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"

{- "-36..17" -}
toSegment :: T.Text -> Segment
toSegment pairStr = Segment $ toTuple $ map readInt $ T.splitOn ".." pairStr

augment :: Segment -> Segment -> ResultType
augment s1 s2 =
  undefined

reduce :: Segment -> Segment -> ResultType
reduce s1 s2 =
  undefined