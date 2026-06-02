module Rewriter (rewriteProgram) where


import Grammar


import Data.Bits ((.|.), shiftL)


rewriteProgram :: Program -> Program
rewriteProgram stats = stats1
  where stats1 = rewriteSet2 stats


rewriteSet2 :: Program -> Program
rewriteSet2 [] = []
rewriteSet2 (InstStat (SetInst rd1 (LabelImm kind1 label1)) :
             InstStat (SetInst rd2 (LabelImm kind2 label2)) :
             rest)
  | areAdjacent && sameLabel = InstStat (Set2Inst rd1 rd2 (LabelImm Full label1)) : rewriteSet2 rest
  where areAdjacent = (kind1 == Low && kind2 == High) || (kind1 == High && kind2 == Low)
        sameLabel   = label1 == label2
rewriteSet2 (InstStat (SetInst rd1 (IntImm Full int1)) :
             InstStat (SetInst rd2 (IntImm Full int2)) :
             rest)
  = InstStat (Set2Inst rd1 rd2 (IntImm Full fullInt)) : rewriteSet2 rest
  where fullInt = (int1 `shiftL` 8) .|. int2
rewriteSet2 (stat : stats) = stat : rewriteSet2 stats
