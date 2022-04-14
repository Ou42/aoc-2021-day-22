{-# LANGUAGE OverloadedStrings #-}

module Segment where

import qualified Data.Text as T

import Utils (readInt, toTuple)

newtype Segment = Segment (Int, Int) deriving (Eq) -- fst <= snd

instance Show Segment where
   show (Segment s) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"

{- "-36..17" -}
toSegment :: T.Text -> Segment
toSegment pairStr = Segment $ toTuple $ map readInt $ T.splitOn ".." pairStr
