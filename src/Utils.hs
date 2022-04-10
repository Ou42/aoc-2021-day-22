module Utils where

{- | Traverse a list whose each entry consists of an `operator`
   | and an `operand`.  The `operator` includes a function as well
   | as well as a source object.  The `operand` contains a target object.
   |
   | COMMENT/TODO: I'm trying to generalize the solution for how to generate the sequence
   | of reb...TODO
-}
sequenceSteps :: [ (opr, opnd) ] -> [ (f acc -> opr -> opnd -> acc) ]
sequenceSteps [operator, operand] = [] -- TODO
