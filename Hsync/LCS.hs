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
