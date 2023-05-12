module Inversions where
-- https://gist.github.com/MastaP/2035851#file-inversions-hs
    
import Data.List (elem, permutations)

mergesort []      = []
mergesort a@[_] = a
mergesort xs      = let (ls,rs) = split xs in merge (mergesort ls) (mergesort rs)

merge [] rs                 = rs
merge ls []                 = ls
merge ls@(l:lst) rs@(r:rst) = if l < r
                              then l:merge lst rs
                              else r:merge ls rst

split []       = ([],[])
split [x]      = ([x],[])
split (l:r:xs) = let (ls,rs) = split xs in (l:ls,r:rs)

split' []       = ([],[])
split' [x]      = ([x],[])
split' xs = let q = div (length xs) 2 in splitAt q xs

sortandcount []      = ([],0)
sortandcount a@[_] = (a,0)
sortandcount xs      = (xs',lc + rc + xc) where
                       (ls,rs) = split' xs
                       (ls',lc) = sortandcount ls
                       (rs',rc) = sortandcount rs
                       (xs',xc) = mergeandcount ls' rs'

mergeandcount [] rs                 = (rs,0)
mergeandcount ls []                 = (ls,0)
mergeandcount ls@(l:lst) rs@(r:rst) = if l < r
                              then let (ds,dc) = mergeandcount lst rs in (l:ds,dc)
                              else let (ds,dc) = mergeandcount ls rst in (r:ds,dc+length ls)

countInversions :: Ord a => [a] -> Int
countInversions = snd . sortandcount
