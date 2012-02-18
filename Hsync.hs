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

--------- testing code

-- | Pretend the client has @c@ and the server has @s@ prints a line
-- per block indicating whether the block is the same, different or
-- new.
test :: FilePath -> FilePath -> IO ()
test c s = do
  putStrLn $ "Comparing client file " ++ c ++ " and server file " ++ s
  dl <- diffVector c s
  mapM_ (\(i,jd) -> case jd of
           Just True -> putStrLn $ "offset " ++ show i ++ " same"
           Just False -> putStrLn $ "offset " ++ show i ++ " differs"
           Nothing -> putStrLn $ "offset " ++ show i ++ " new")
    (zip [1..] dl)
  
-- | Return a list of two files @fa@ and @fb@ showing which offset
-- differ. @Just True@ indicates the blocks are the same at this offset,
-- @Just False@ means they differ. @Nothing@ indicates that only one of
-- the files has a block for this offset.
diffVector :: FilePath -> FilePath -> IO [Maybe Bool]
diffVector fa fb = do
  bhas <- getBlockHashes fa
  bhbs <- getBlockHashes fb
  return $ build [] bhas bhbs
    where build accum as bs = case (null as, null bs) of
            (True, True) -> reverse accum
            (False, True) -> build (Nothing:accum) (tail as) bs
            (True, False) -> build (Nothing:accum) as (tail bs)
            (False, False) -> build ((if (head as == (head bs)) 
                                      then Just True 
                                      else Just False):accum) (tail as) (tail bs)

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
