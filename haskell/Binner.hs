import Bio.Core.Sequence
import Bio.Sequence.Fasta
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap as Map
import qualified Data.Map as DM
import System.Directory
import System.IO
import System.Environment
import System.FilePath.Posix (combine, takeFileName)

main :: IO ()
main = do
  files <- getArgs
  mapM_ process files
  putStrLn "Done."

process file = do
  let fileName = takeFileName file

  putStrLn $ "Processing " ++ fileName

  reads <- readFasta file
  let kmers   = concatMap (findKmers 20 . unSD . seqdata) reads
  let allMers = replicateM 5 "ACTG"
  let outDir  = combine "out" fileName
  outExists <- doesDirectoryExist outDir
  unless outExists (createDirectoryIfMissing True outDir)

  fileHandles <- 
    Map.fromList `fmap` mapM (
      \x -> do 
        h <- openFile (combine outDir x) WriteMode 
        return (x,h)
      ) allMers

  mapM_ (\kmer -> do
    let bin = toString $ B.take 5 kmer
    let h   = Map.lookup bin fileHandles
    case h of
      Just handle -> B.hPutStrLn handle $ B.drop 5 kmer
      Nothing -> putStrLn $ "No handle for " ++ bin
    ) kmers 

  mapM_ hClose $ Map.elems fileHandles

-- # --------------------------------------------------
findKmers :: Integer -> B.ByteString -> [B.ByteString]
findKmers k xs = findKmers' n k xs
  where n = toInteger (B.length xs) - k + 1
        findKmers' n' k' xs'
          | n' > 0 = B.take (fromIntegral k') xs'
             : findKmers' (n' - 1) k' (B.tail xs')
          | otherwise = []
