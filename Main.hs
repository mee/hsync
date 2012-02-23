module Main where

import Hsync
import Hsync.LCS

main :: IO ()
main = do let as = [1,3..1999]
              bs = [1..2000] in putStrLn $ show $ lcs as bs