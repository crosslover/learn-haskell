module MaxAlternatingSum (maxAlternatingSum) where

import GHC.Base (minInt)

maxAlternatingSum :: [Int] -> Int
maxAlternatingSum = fst . doMaxAlternatingSum . reverse

doMaxAlternatingSum :: [Int] -> (Int, Int)
doMaxAlternatingSum [] = (0, minInt)
doMaxAlternatingSum (a : left) = (max r (max a (a + withP)), max withP (r - a))
  where
    (r, withP) = doMaxAlternatingSum left