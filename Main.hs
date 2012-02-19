module Main where

import Hsync

main :: IO ()
main = do let as = [1,3..999]
              bs = [1..1000] in putStrLn $ show $ lcs as bs