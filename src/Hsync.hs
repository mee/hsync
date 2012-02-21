module Hsync where

import Hsync.LCS
import Codec.Utils
import Data.Digest.MD5
import System.FilePath.Posix
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w,w2c)
import System.IO

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
  return $! map c2w . BL.unpack $ bs
  
-- | write given block at given offset in-place
writeBlock :: Handle -> Integer -> [Octet] -> IO ()
writeBlock hdl off blk = do
  pos <- hGetPosn hdl
  hSeek hdl AbsoluteSeek (bs*off)
  BL.hPut hdl ( BL.pack $ map w2c blk )

-- | return a list of block hashes for a given file
getBlockHashes :: FilePath -> IO [[Octet]]
getBlockHashes fp = do bl <- getBlockList fp
                       return $ map hash bl

-- | return a list of blocks for each block in a file @fp@
getBlockList :: FilePath -> IO [[Octet]]
getBlockList fp = 
  let numBlocks = (\eof -> ceiling $ fromRational $ (fromIntegral eof) / (fromIntegral bs)) in
    withFile fp ReadMode (\hdl -> do
                             eof <- hFileSize hdl
                             mapM (getBlock hdl) [0..(numBlocks eof - 1)] )

-- Patch-related code

data Patch = Change (Integer,[Octet])
           | Delete Integer
           | Insert (Integer,[Octet])
             
instance Show Patch where             
  show (Change (i,_)) = "Change " ++ show i
  show (Delete i)     = "Delete " ++ show i
  show (Insert (i,_)) = "Insert " ++ show i

-- | patch @fb@ to match @fa@
patch :: FilePath -> FilePath -> IO ()
patch fa fb = do
  bla <- getBlockList fa
  blb <- getBlockList fb
  let bha = map hash bla
      bhb = map hash blb
      cbs = lcs bha bhb
      pb2a = mkPatch cbs bla bha bhb
  putStrLn $ "ha:  " ++ show bha
  putStrLn $ "hb:  " ++ show bhb
  putStrLn $ "lcs: " ++ show cbs
  putStrLn $ "pl:  " ++ show pb2a
--  applyPatch pb2a fb
      

-- | Given the LCS of block lists between two files, and the source
-- files path, generate a patch to convert the destination file to the
-- source.
mkPatch :: [[Octet]]      -- ^ LCS of hashes
           -> [[Octet]]   -- ^ blocks from source
           -> [[Octet]]   -- ^ hashes from source
           -> [[Octet]]   -- ^ hashes from dest
           -> [Patch]     -- ^ returned patch
mkPatch lcs blks hshs hshd = step 0 [] lcs blks hshs hshd
  where step :: Integer       -- ^ block counter
                -> [Patch]    -- ^ accumulator list
                -> [[Octet]]  -- ^ LCS of hashes
                -> [[Octet]]  -- ^ block list from source file
                -> [[Octet]]  -- ^ hash list from source file
                -> [[Octet]]  -- ^ hash list from dest file
                -> [Patch]    -- ^ returned patch
        step _ pl _ [] [] _ = reverse pl -- out of source blocks and hashes, must be done
        step _ pl lcs@(c:cs) [] _ _ = error "common element remaining, but blocks exhausted."
        step _ pl _ [] hshs@(hs:hss) _ = error "block list exhausted, but source hashes remain."
        step i pl [] blks@(b:bs) hshs@(hs:hss) hshd@(hd:hsd) = step (i+1) ((Change (i,b)):pl) [] bs hss hsd
        step i pl [] blks hshs [] = (reverse pl) ++ map (\(i,b) -> Insert (i,b)) (zip [i..] blks)
        step i pl lcs@(c:cs) blks@(b:bs) hshs@(hs:hss) hshd@(hd:hsd) = 
          if c == hd && c == hs             -- hit common element, skip
          then step (i+1) pl cs bs hss hsd
          else if c == hd && c /= hs        -- found element before common item in d but not c, insert
               then step (i+1) ((Insert (i,b)):pl) lcs bs hss hsd
               else if c /= hd && c == hs   -- found element before common item in c but not d, delete
                    then step (i+1) ((Delete i):pl) lcs blks hshs hsd
                    else step (i+1) ((Change (i,b)):pl) lcs bs hss hsd -- before common item in both, change

  
-- | Apply a patch to a file. Write to copy of file and then
-- (atomically) update the file with rename.
applyPatch :: [Patch] -> FilePath -> IO ()
applyPatch = undefined
