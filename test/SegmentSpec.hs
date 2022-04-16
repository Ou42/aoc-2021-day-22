module SegmentSpec where

import Segment (ResultType(..), Segment(..), augment, compareSegments, reduce)
import SegmentTest (areSame, targetIsAdjacentLeft, targetIsAdjacentRight)

