import Bio.Core.Sequence
import Bio.Sequence.Fasta
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap as Map
import Data.Monoid
import Options.Applicative
import System.Directory
import System.IO
import System.Environment
import System.FilePath.Posix (joinPath, takeFileName, takeBaseName)
import System.FilePath.Find
import Text.Printf (printf)

-- # --------------------------------------------------
data Options = Options {
  optInFile   :: String,
  optInDir    :: String,
  optOutDir   :: String,
  optKmerSize :: Integer,
  optBinSize  :: Int
} deriving (Show)

-- # --------------------------------------------------
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = Options <$> strOption
                         ( long "file"
                         <> short 'f'
                         <> value ""
                         <> metavar "FILE_INPUT" )
                     <*> strOption
                         ( long "in-dir"
                         <> short 'd'
                         <> value ""
                         <> metavar "DIR_INPUT" )
                     <*> strOption
                         ( long "outdir"
                         <> short 'o'
                         <> metavar "DIR_OUT" )
                     <*> option auto
                         ( long "kmer"
                         <> short 'k'
                         <> value 20
                         <> metavar "KMER_SIZE" )
                     <*> option auto
                         ( long "bin"
                         <> short 'b'
                         <> value 5
                         <> metavar "BIN_SIZE" )
    opts = info parser mempty

---- # --------------------------------------------------
runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let outDir = optOutDir opts
  inFiles <- locateFiles (optInFile opts) (optInDir opts)

  print inFiles


  if (null inFiles)
    then putStrLn "Could not find input files"
    else mapM_ (process outDir (optKmerSize opts) (optBinSize opts)) inFiles 

  let n = length inFiles
  printf "Done processing %d file%s\n" n (if (n == 1) then "" else "s")
  
-- # --------------------------------------------------
locateFiles :: FilePath -> FilePath -> IO [FilePath]
locateFiles inFile inDir = do
  inFileExists <- doesFileExist inFile

  if inFileExists
  then return [inFile] 
  else do
    contents <- find always (fileType ==? RegularFile) inDir
    return contents

-- # --------------------------------------------------
process outDir kmerSize binSize file = do
  let fileName = takeFileName file

  putStrLn $ "Processing " ++ show fileName

  reads <- readFasta file
  let kmers   = concatMap (findKmers kmerSize . unSD . seqdata) reads
  let allMers = replicateM binSize "ACTG"
  let dir     = joinPath [outDir, fileName]
  outExists <- doesDirectoryExist dir
  unless outExists (createDirectoryIfMissing True dir)

  fileHandles <- 
    Map.fromList `fmap` mapM (
      \x -> do 
        h <- openFile (joinPath [outDir, x]) WriteMode 
        return (x,h)
      ) allMers

  mapM_ (\kmer -> do
    let bin = toString $ B.take (fromIntegral binSize) kmer
    let h   = Map.lookup bin fileHandles
    case h of
      Just handle -> B.hPutStrLn handle $ B.drop (fromIntegral binSize) kmer
      Nothing -> return ()
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
