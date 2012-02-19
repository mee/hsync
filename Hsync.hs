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

-- for lcs_clrs
import Control.Monad.ST
import Data.Array.ST
import GHC.Arr
import Control.Monad

data Patch = Change (Integer,[Octet])
           | Remove (Integer)

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

-- | Find the longest common subsequence of two lists.
lcsSlow :: (Ord a, Eq a) => [a] -> [a] -> [a]
lcsSlow = build []
  where build l (a:as) (b:bs) | a == b = build (a:l) as bs
                              | otherwise = biggest [ build l (a:as) bs
                                                    , build l as (b:bs) ]
        build l [] _ = reverse l
        build l _ [] = reverse l
        
lcs as bs = let arr = lcsDir as bs        
                  (_,(mx,my)) = bounds arr in
              step [] arr as (mx-1,my-1)
  where step l a as (x,y) = let d = snd (a!(x,y)) in 
          if (or [ x == 0, y == 0 ])
            then l
            else step ((as!!(x-1)):l) a as (case snd (a!(x,y)) of
                                           1 -> (x,y-1)
                                           2 -> (x-1,y-1)
                                           3 -> (x-1,y)
                                           _ -> error (show (x,y)))

lcsLen as bs = let arr = lcsDir as bs
                   (_,(mx,my)) = bounds arr
                   (v,_) = arr!(mx-1,my-1) in v

-- | pretty-print our direction array
ppDirArr :: (Show a) => [a] -> [a] ->  Array (Int,Int) (Int, Int) -> IO ()
ppDirArr as bs arr = let idx = (\x y -> (x,y))
                         (_,(lx,ly)) = bounds arr 
                         colw = 1 + length (show (as!!1)) in do
  putStrLn $ pad colw " " ++ (concat $ map (pad (1+colw)) ("b":(map show bs)))
  mapM_ (\x -> do putStr (if x == 0 then (pad (colw-1) "a") else show (as!!(x-1)))
                  mapM_ (\y -> do case arr!(x, y) of
                                    (l,0) -> putStr $ " " ++ pad colw (show l)
                                    (l,1) -> putStr $ "↑" ++ pad colw (show l)
                                    (l,2) -> putStr $ "↖" ++ pad colw (show l)
                                    (l,3) -> putStr $ "←" ++ pad colw (show l)) [0..(ly-1)]
                  putStrLn "" ) [0..(lx-1)]
    where pad n s = let l = length s in if l >= n then s else s ++ (concat $ replicate (n-l) " ")

-- data Dir = W | NW | N | X -- deriving (MArray Int Dir m)
--            3   2    1   0          
lcsDir :: (Ord a) => [a] -> [a] -> Array (Int,Int) (Int,Int) -- (length,dir)
lcsDir as bs = build as bs
   where build as bs = let la = length as
                           lb = length bs in runSTArray $ do
                         d <- newArray ((0,0),(la+1,lb+1)) (0,0)
                         mapM_ (\ia -> mapM_ (\ib -> point d as ia la bs ib lb) [1..lb]) [1..la]
                         return d
         point d as ia la bs ib lb = do (e, _) <- readArray d (ia, ib)
                                        (le,_) <- readArray d (ia-1, ib)
                                        (ue,_) <- readArray d (ia, ib-1)
                                        if as!!(ia-1) == bs!!(ib-1) -- as, bs are zero-origin
                                         then do (de,_) <- readArray d (ia-1, ib-1)
                                                 writeArray d (ia, ib) (de+1,2)
                                         else if le >= ue
                                              then do writeArray d (ia, ib) (le,1)
                                              else do writeArray d (ia, ib) (ue,3)

biggest = foldr (\a b -> if length a > length b then a else b) []                            

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
