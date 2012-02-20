module Main where

-- import Data.Vector

main = let as = [1,3..999]
           bs = [1..1000] in pp (1 + length bs) $ lcsDir as bs
                             
-- generate :: Int -> (Int -> a) -> Vector a
       
data Dir = W | D | N | X deriving (Ord, Eq, Show)

lcsDir :: (Eq a) => [a] -> [a] -> [(Int,Dir)]
lcsDir as bs = let la = length as
                   lb = length bs
                   idx r c = (1+lb)*r + c 
                   zs = replicate (lb+1) (0,X) ++ (concat $ map (\r -> map (\c -> if c == 0 then (0,X) else 
                                             let le = fst (zs!!(idx (r-1) c))
                                                 ue = fst (zs!!(idx r (c-1)))
                                                 de = fst (zs!!(idx (r-1) (c-1))) in
                                                    if as!!(r-1) == bs!!(c-1)
                                                      then (de+1, D)
                                                      else if le >= ue
                                                             then (le, N)
                                                             else (ue, W) ) [0..lb] ) [1..la]) in take ((la+1)*(lb+1)) zs

pp :: (Show a, Show b) => Int -> [(a,b)] -> IO ()
pp rl zs = mapM_ (putStrLn . pshow) $ brk zs
           where brk l | length l <= rl = [l]
                       | otherwise = (take rl l) : ( brk (drop rl l) )
                 pad k s = if length s >= k then s else s ++ (replicate (k - (length s)) ' ')
                 pshow :: (Show a, Show b) => [(a,b)] -> String
                 pshow vs = concat $ map (\(v,d) -> (show d ++ pad 3 (show v) ++ " ")) vs

