{-# LANGUAGE OverloadedStrings #-}

module Cuboid where

import qualified Data.Text as T

import Segment (Segment(..), SrcSeg(..), TrgSeg(..), convert, dimension, toSrcSeg)

newtype Source = Source [SrcSeg] deriving (Eq, Show)

newtype Target = Target [TrgSeg] deriving (Eq, Show)

{- | This routine is needed because, the comparison of Source and Target cuboids
   | is very precisely managed in order to enable the compiler to prevent inadvertant Source comparisons
   | against other sources which could be difficult to track down. (I.e. make situations
   | you want to be impossible, well, add coding to make them impossible.) This paid off; the
   | debugging of the calculations was straightforward and efficient.
   |
   | But, after we check a Source against all other previously processed targets, this Source
   | needs to "turn into " a Target because the subsequent Source's will need to be checked
   | against this Source in turn.  This function provides that functionality.
-}
convertFromSourceToTarget :: Source -> Target
convertFromSourceToTarget (Source source) =
  Target $ map convert source

volume :: Target -> Int
volume (Target sides) = product $ map dimension sides

{- "x=-20..26,y=-36..17,z=-47..7" -}
parseSource :: T.Text -> Source
parseSource str =
    Source $ map (toSrcSeg . last . T.splitOn "=") $ T.splitOn "," str
