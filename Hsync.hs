module Hsync where

import Codec.Utils -- Octet, fromOctets, toOctets
import Data.Digest.MD5 -- hash :: [Octet] -> [Octet]
import System.FilePath.Posix -- FilePath
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bits.Utils (c2w8,w82c)
import System.IO
import Control.Monad
import System.Cmd -- for test setup
import Data.List -- permutations
import Control.Applicative -- liftM

main :: IO ()
main = do test "test/edfhcibgja" "test/bijchefdga"

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

-- | write given block at given offset
writeBlock :: Handle -> Integer -> [Octet] -> IO ()
writeBlock hdl off blk = do
  pos <- hGetPosn hdl
  hSeek hdl AbsoluteSeek (bs*off)
  BL.hPut hdl ( BL.pack $ map w82c blk )

-- | return a list of hashes for each block in a file @fp@
getBlockHashes :: FilePath -> IO [[Octet]]
getBlockHashes fp = 
  let numBlocks = (\eof -> ceiling $ fromRational $ (fromIntegral eof) / (fromIntegral bs)) in
    withFile fp ReadMode (\hdl -> do
                             eof <- hFileSize hdl
                             mapM (getBlock hdl) [0..(numBlocks eof - 1)] )

-- | update file @fa@ by truncating it to the given size and
-- overwriting the blocks at the specified offsets with the ones
-- given.
patch :: FilePath -> (Integer,[(Integer,[Octet])]) -> IO ()
patch fa idl = let (sz,dl) = idl in withFile fa ReadWriteMode
         (\hdl -> do eof <- hFileSize hdl
                     when (eof /= sz) (hSetFileSize hdl sz)
                     mapM_ (\(i,d) -> writeBlock hdl i d) dl)

-- | like DiffVector, but the list is only as long as the @fa@. This
-- is handy for computing which blocks you need to send from @fa@ to
-- patch @fb@.
minDiffVector :: FilePath -> FilePath -> IO [Maybe Bool]
minDiffVector fa fb = do
  bhas <- getBlockHashes fa
  bhbs <- getBlockHashes fb
  return $ build [] bhas bhbs
    where build accum as bs = case (null as, null bs) of
            (True, True) -> reverse accum
            (False, True) -> build (Nothing:accum) (tail as) bs
            (True, False) -> reverse accum
            (False, False) -> build ((if (head as == (head bs)) 
                                      then Just True 
                                      else Just False):accum) (tail as) (tail bs)
                              

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

-- | Given a difference vector @dv and the server file @fp@, return
-- the indexed difference list
dv2idl :: FilePath -> [Maybe Bool] -> IO (Integer,[(Integer,[Octet])])
dv2idl fp dv = let idv = filter (\(i,d) -> case d of
                                 Just True -> False
                                 otherwise -> True) $ zip [0..] dv in
  withFile fp ReadMode 
    (\hdl -> do eof <- hFileSize hdl
                lst <- mapM (\(i,d) -> liftM ((,) i) (getBlock hdl i)) idv
                return (eof,lst) )


--------- testing code

-- | Pretend the client has @c@ and the server has @s@ prints a line
-- per block indicating whether the block is the same, different or
-- new.
testEcho :: FilePath -> FilePath -> IO ()
testEcho c s = do
  putStrLn $ "Comparing client file " ++ c ++ " and server file " ++ s
  dl <- diffVector c s
  mapM_ (\(i,jd) -> case jd of
           Just True -> putStrLn $ "offset " ++ show i ++ " same"
           Just False -> putStrLn $ "offset " ++ show i ++ " differs"
           Nothing -> putStrLn $ "offset " ++ show i ++ " new")
    (zip [1..] dl)

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

test :: FilePath -> FilePath -> IO ()
test s c = do dv <- minDiffVector s c -- find where c is different from s (length dv == (length s) / bs)
              putStrLn $ "dv: " ++ show dv
              idl@(sz,dl) <- dv2idl s dv -- get the blocks we need from s to patch c
              putStrLn $ "indices: " ++ show (map fst dl)
              patch c idl
