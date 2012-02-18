module Hsync where

import Codec.Utils -- Octet, fromOctets, toOctets
import Data.Digest.MD5 -- hash :: [Octet] -> [Octet]
import System.FilePath.Posix -- FilePath
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bits.Utils (c2w8)
import System.IO
import Control.Monad
import System.Cmd -- for test setup
import Data.List -- permutations

main = undefined

bs :: Integer
bs = 1024*4 -- 4 kB

-- | return the raw data from a handle @hdl@ from @Offset@ to @Offset + blocksize - 1@
getBlock :: Handle -> Integer -> IO [Octet]
getBlock hdl o = let off = (\e -> minimum [(o*bs), e])       -- begining of block
                     end = (\e -> minimum [(bs + off e), e]) -- end of block
                     len = (\e -> end e - off e) in do       -- size of block
  pos <- hGetPosn hdl
  eof <- hFileSize hdl
  hSeek hdl AbsoluteSeek (off eof)
  bs <- BL.hGet hdl (fromIntegral $ len eof)
  hSetPosn pos
  return $! map c2w8 . BL.unpack $ bs

-- | return a list of hashes for each block in a file @fp@
getBlockHashes :: FilePath -> IO [[Octet]]
getBlockHashes fp = 
  let numBlocks = (\eof -> ceiling $ fromRational $ (fromIntegral eof) / (fromIntegral bs)) in
    withFile fp ReadMode (\hdl -> do
                             eof <- hFileSize hdl
                             mapM (getBlock hdl) [0..(numBlocks eof - 1)] )

-- | Pretend the client has @c@ and the server has @s@
test :: FilePath -> FilePath -> IO ()
test c s = do
  putStrLn $ "Comparing client file " ++ c ++ " and server file " ++ s
  bhc <- getBlockHashes c
  bhs <- getBlockHashes s
  let diffList = zip3 ([0..]::[Integer]) bhc bhs
  mapM_ sendBlock diffList
    where
      sendBlock (i,ha,hb) = do
        putStrLn $ "offset " ++ show i ++ (if (ha == hb) then " same" else " diff")

-- | Return a list of two files @fa@ and @fb@ showing which offset
-- differ. @True@ indicates the blocks are the same at this offset,
-- @False@ means they differ.
diffVector :: FilePath -> FilePath -> IO [Bool]
diffVector fa fb = do
  bha <- getBlockHashes fa
  bhb <- getBlockHashes fb
  return $ map (\(ha,hb) -> if (ha == hb) then True else False) $ zip bha bhb


-- | take two times @numidx@ permutations of @chunks@ and create them as files, where
-- each letter corresponds to a unique binary block. Then verify that the diff vectors
-- of the files corresponds to the file names
makeTestFiles :: IO ()
makeTestFiles = do
  mapM_ (\c -> rawSystem "dd" ["if=/dev/urandom","bs=4k","count=1","of=test/" ++ [c]]) chunks
  mapM_ putStrLn cmds
    where chunks = "abcdefghij"
          numidx = 6
          peroff = div (foldr (*) 1 [1..(length chunks)]) numidx
          indices = concat $ map (\i -> [peroff*i,3+peroff*i]) [1..numidx]
          permutes = zip [1..] (permutations chunks) 
          cmds = map (\(i,c) -> "cat " ++ intersperse ' ' c ++ " > " ++ c) $
                 filter (\(i,c) -> if i `elem` indices then True else False) permutes
