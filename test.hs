module Main where

import qualified Data.Vector as V

main = let as = [1,3..9]
           bs = [1..10]
           le = V.last $ lcsDir as bs in do putStrLn $ "as: " ++ show as
                                            putStrLn $ "bs: " ++ show bs
                                            putStrLn $ "length: " ++ show (fst le)

-- This hits an infinite loop with unboxed vectors because the
-- concatenation doesn't seem to happen until after the second part is
-- built, so r=1,c=1 fails when it tries to access the first element
-- of the list.
lcsDir :: (Eq a) => [a] -> [a] -> V.Vector (Int,Char)
lcsDir as bs = let la = length as
                   lb = length bs
                   idx r c = (lb+1)*r + c
                   zs = (V.++) (V.replicate (lb+1) (0,'-')) 
                               (V.generate (la*(lb+1)) 
                                (\i -> let (r',c) = i `quotRem` (lb+1) in
                                  if c == 0 then (0,'X') else
                                    let r = r' + 1 in
                                    if as!!(r-1) == bs!!(c-1)
                                    then let de = fst ((V.!) zs (idx (r-1) (c-1))) in (de+1, 'D')
                                    else let le = fst ((V.!) zs (idx (r-1) c))
                                             ue = fst ((V.!) zs (idx r (c-1))) in
                                         if le >= ue
                                         then (le, 'N')
                                         else (ue, 'W') ) ) in zs

pp :: Int -> [(Int,Char)] -> IO ()
pp rl zs = mapM_ (putStrLn . rshow) $ brk zs
  where brk l | length l <= rl = [l]
              | otherwise = take rl l : brk (drop rl l)
        pad k s = let ls = length s in if ls >= k then s else s ++ replicate (k - ls) ' '
        rshow = concatMap (\(v,d) -> (d:[] ++ pad 3 (show v) ++ " "))

