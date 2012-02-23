module Hsync.LCS (lcs) where

import qualified Data.Vector as V
import Data.List -- nub, sort
-- import Test.QuickCheck

-- This hits an infinite loop with unboxed vectors because the
-- concatenation doesn't seem to happen until after the second part is
-- built (or something else strange), so r=1,c=1 fails when it tries
-- to access the first element of the list.
lcsDir :: (Eq a) => [a] -> [a] -> V.Vector (Int,Char)
lcsDir as bs = let la = length as
                   lb = length bs
                   idx r c = (lb+1)*r + c
                   zs = (V.++) (V.replicate (lb+1) (0,' '))
                               (V.generate (la*(lb+1))
                                (\i -> let (r',c) = i `quotRem` (lb+1) in
                                  if c == 0 then (0,' ') else
                                    let r = r' + 1 in
                                    if as!!(r-1) == bs!!(c-1)
                                    then let de = fst ((V.!) zs (idx (r-1) (c-1))) in (de+1, '↖')
                                    else let le = fst ((V.!) zs (idx (r-1) c))
                                             ue = fst ((V.!) zs (idx r (c-1))) in
                                         if le >= ue
                                         then (le, '↑')
                                         else (ue, '←') ) ) in zs

{- Instead of building the lcs table scanning top left to bottom
right, build as a “wave front”, with each generation of the algorithm
adding a new bottom-left to top-right diagonal. For example, consider
this table where the cells indicate the hexidecimal generation number
that the cell value was computed in:

  0000000000   For large inputs, it makes sense to parallelize
  0123456789   the creation of these diagonals. The intermediate
  023456789A   state for this algorithm will be n+m lists of
  03456789AB   size 1,2,…,(min(n,m)){max(n,m)-min(n,m)},…,2,1
  0456789ABC   where n,m are the lengths of the input lists and
  056789ABCD   whose end values (at least) are initialized to zero.
  06789ABCDE   For small inputs, continue to process sequentially.

maxl = max(n,m) = 9  -- max of a, b length
minl = min(n,m) = 6  -- min of a, b length aka max diagonal length
nmdl = maxl - minl   -- num of max diagonals

Up region g=1..minl
at g=3, r=3..1, c=1..3   -->  r=g..1
at g=4, r=4..1, c=1..4   -->  c=1..g

Flat region g=minl..(minl+(nmdl-1))
at g=7, r=6..1, c=2..7   -->  r=minl..1
at g=8, r=6..1, c=3..8   -->  c=g-minl+1..g

Down region g=(minl+nmdl)..
at g=B, r=6..3, c=6..9   --> r=minl..(g-maxl+1)
at g=C, r=6..4, c=7..9   --> c=(g-minl+1)..maxl

Let's test these theories on a smaller table, broken into regions,

  000 / 00 /     minl=2
  01 / 23 / 4    maxl=4
  0 / 23 / 45    nmdl=2

Up region
g=1, r=1..1, c=1..1             -> (1,1)

Flat region
g=2, r=2..1, c=(2-2+1)..2       -> (2,1), (1,2)
g=3  r=2..1, c=(3-2+1)..3       -> (2,2), (1,3)

Down region
g=4  r=2..(4-4+1), c=(4-2+1)..4 -> (2,3), (1,4)
g=5  r=2..(5-4+1), c=(5-2+1)..4 -> (2,4)           ..OK!

Note this will only work for when the box is at least as wide as it is
tall, so lcsDirWave as bs requires length bs >= length as. -}

type VIC = V.Vector (Int,Char)
-- lcsDirWave :: (Eq a) => [a] -> [a] -> VIC
lcsDirWave as bs | length as < length bs = lcsDirWave bs as
                 | otherwise = let la = length as
                                   lb = length bs
                                   minl = minimum [ la, lb ]                   -- aka max diag length
                                   maxl = maximum [ la, lb ]
                                   nmdl = maxl - minl
                                   gma = V.replicate 1 (0,' ')                 -- two generations ago (grandma)
                                   mom = V.replicate 2 (0,' ')                 -- one generation ago (mom) 
                                   gsUp = [1..minl]                            -- generations in up region
                                   gsFl = [minl..(maxl-1)]                     -- generations in flat region
                                   gsDn = [maxl..1]                            -- generationn in down region
                                   rsUp = map (\g -> [g..1]) gsUp              -- rows iterated over in up region for each generation
                                   csUp = map (\g -> [1..g]) gsUp              -- cols iterated over in up region for each generation
                                   rsFl = map (\g -> [minl..1]) gsFl           -- rows "" flat "" 
                                   csFl = map (\g -> [g-minl+1..g]) gsFl       -- cols "" flat ""
                                   rsDn = map (\g -> [minl..g-maxl+1]) gsDn    -- rows "" down ""
                                   csDn = map (\g -> [g-minl+1..maxl]) gsDn    -- cols "" down ""
                                   ups = scanl (wave as bs rsUp csUp) (gma,mom) gsUp -- TODO: use V.scanl
                                   fls = scanl (wave as bs rsFl csFl) ((snd . last $ ups),(snd . last $ ups)) gsFl
                                   dns = scanl (wave as bs rsDn csDn) ((snd . last $ fls),(snd . last $ fls)) gsDn in 
                               (ups,fls,dns) -- TODO: return just the waves
  where wave as bs rs cs (gma,mom) n = let rows = rs!!n      -- TODO: if n > branching threshold,
                                           cols = cs!!n in   -- divide rs, cs, n and //ize!
                                       (,) mom $ V.generate n 
                                       (\i -> let r = rows!!i
                                                  c = cols!!i in
                                              if as!!(r-1) == bs!!(c-1) -- blowing up at g=1,r=1,c=1
                                              then let de = fst ((V.!) gma (i+1)) in (de+1, '↖')
                                              else let le = fst ((V.!) mom i)
                                                       ue = fst ((V.!) mom (i+1)) in
                                                   if le >= ue
                                                   then (le, '↑')
                                                   else (ue, '←') )

lcs :: (Eq a) => [a] -> [a] -> [a]
lcs as bs = let arr = lcsDir as bs
                rl = length bs
                (mr,mc) = (`quotRem` (rl+1)) . pred . V.length $ arr in step [] arr as rl mr mc
  where step acc arr as rl r c = if or [ r == 0, c == 0 ] then acc else
                                   case (V.!) arr ((1+rl)*r+c) of
                                     (_,'↖') -> step ((as!!(r-1)):acc) arr as rl (r-1) (c-1)
                                     (_,'←') -> step acc arr as rl r (c-1)
                                     (_,'↑') -> step acc arr as rl (r-1) c
                                     otherwise -> error "malformed table"

-- the lcs must be <= as long as either parent list
prop_lcs_len as bs = let ss = lcs as bs
                         ls = length ss
                         la = length as
                         lb = length bs in and [ ls <= la, ls <= lb ]

-- every lcs is a subsequence of it's parents
prop_lcs_elem as bs = let ss = lcs as bs in all (\e -> and [ e `elem` as, e `elem` bs ] ) ss

-- every lcs has elements in the same order as it's parents
prop_lcs_order as bs = let ss = lcs as bs in and [ step ss as, step ss bs ]
  where step (s:ss) (v:vs) | s == v = step ss vs
                           | s /= v = step (s:ss) vs
        step [s] [] = False
        step [] _ = True

-- items occur in the lcs <= as many times as they occur in either parent string
prop_lcs_mult as bs = let ss = lcs as bs
                          fa = freqs as
                          fb = freqs bs
                          fs = freqs ss in and [ freqCmp fs fa, freqCmp fs fb ]
  where freqs ls = let ue = nub ls in sort $ map (\e -> (e, length $ filter (==e) ls)) ue
        freqCmp fs@((se,sc):sec) fa@((ae,ac):aec) | se == ae = and [ sc <= ac , freqCmp sec aec ]
                                                  | se /= ae = freqCmp fs aec
        freqCmp fs@((se,sc):[]) ((ae,ac):aec) | se == ae = sc <= ac
                                              | se /= ae = freqCmp fs aec
        freqCmp [] as = True   -- there were no common elements
        freqCmp fs [] = False  -- this actually fails prop_lcs_elem

-- example tests:
-- quickCheck (prop_lcs_len :: (Num a) => [a] -> [a] -> Bool)
-- quickCheck (prop_lcs_elem :: (Num a) => [a] -> [a] -> Bool)
-- quickCheck (prop_lcs_order :: (Num a) => [a] -> [a] -> Bool)
-- quickCheck (prop_lcs_mult :: (Num a) => [a] -> [a] -> Bool)
-- quickCheck (prop_lcs_len :: [Char] -> [Char] -> Bool)
-- quickCheck (prop_lcs_elem :: [Char] -> [Char] -> Bool)
-- quickCheck (prop_lcs_order :: [Char] -> [Char] -> Bool)
-- quickCheck (prop_lcs_mult :: [Char] -> [Char] -> Bool)

-- ex usage: let as = [1,3..9]; bs = [1..10] in pp (1+length as) $ V.toList $ lcsDir as bs
pp :: Int -> [(Int,Char)] -> IO ()
pp rl zs = mapM_ (putStrLn . rshow) (brk zs)
  where brk l | length l <= rl = [l]
              | otherwise = take rl l : brk (drop rl l)
        pad k s = let ls = length s in if ls >= k then s else s ++ replicate (k - ls) ' '
        rshow = concatMap (\(v,d) -> (d:[] ++ pad 3 (show v) ++ " "))
