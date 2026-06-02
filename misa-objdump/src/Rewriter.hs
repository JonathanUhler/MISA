module Rewriter (rewriteProgram) where


import Grammar


rewriteProgram :: Program -> Program
rewriteProgram stats = stats1
  where stats1 = rewriteSet2 stats


rewriteSet2 :: Program -> Program
rewriteSet2 [] = []
rewriteSet2 (InstStat (SetInst rd1 (LabelImm Low  label1)) :
             InstStat (SetInst rd2 (LabelImm High label2)) :
             rest)
  | label1 == label2 = InstStat (Set2Inst rd1 rd2 (LabelImm Full label1)) : rewriteSet2 rest
rewriteSet2 (stat : stats) = stat : rewriteSet2 stats
