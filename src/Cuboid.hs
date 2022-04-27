{-# LANGUAGE OverloadedStrings #-}

module Cuboid where

import qualified Data.Text as T

import Segment (Segment(..), SrcSeg(..), TrgSeg(..), convert, dimension, toSrcSeg)

newtype Source = Source [SrcSeg] deriving (Eq, Show)

newtype Target = Target [TrgSeg] deriving (Eq, Show)

convertFromSourceToTarget :: Source -> Target
convertFromSourceToTarget (Source source) =
  Target $ map convert source

volume :: Target -> Int
volume (Target sides) = product $ map dimension sides

-- vol2 :: (TrgSeg -> Int) -> Target -> Int
-- vol2 dimFunc (Target sides) = product $ map dimFunc sides


{- "x=-20..26,y=-36..17,z=-47..7" -}
parseSource :: T.Text -> Source
parseSource str =
    Source $ map (toSrcSeg . last . T.splitOn "=") $ T.splitOn "," str
