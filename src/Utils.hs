-- {-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Text as T

readInt :: T.Text -> Int
readInt s = read (T.unpack s) :: Int

toTuple :: [a] -> (a, a)
toTuple (x1:(x2:xs)) = (x1, x2)
