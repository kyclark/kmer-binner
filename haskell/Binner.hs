import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap as Map
import Data.Monoid
import Options.Applicative
import System.Directory
import System.IO
import System.Environment
import System.FilePath.Posix (joinPath)

-- # --------------------------------------------------
data Options = Options {
  optOutDir  :: String,
  optBinSize :: Int
} deriving (Show)

-- # --------------------------------------------------
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = Options <$> strOption
                         ( long "outdir"
                         <> short 'o'
                         <> metavar "DIR_OUT" )
                     <*> option auto
                         ( long "bin"
                         <> short 'b'
                         <> value 5
                         <> metavar "BIN_SIZE" )
    opts = info parser mempty

---- # --------------------------------------------------
runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let binSize = optBinSize opts
  let dir     = optOutDir opts
  outExists  <- doesDirectoryExist dir
  unless outExists (createDirectoryIfMissing True dir)

  let allMers = replicateM binSize "ACTG"

  fileHandles <- 
    Map.fromList `fmap` mapM (
      \x -> do 
        h <- openFile (joinPath [dir, x]) WriteMode 
        return (x,h)
      ) allMers

  input <- B.getContents
  mapM_ (\line -> do
    let kmer = head $ B.words line
    let bin  = B.unpack $ B.take (fromIntegral binSize) kmer
    let h    = Map.lookup bin fileHandles
    case h of
      Just handle -> B.hPutStrLn handle $ B.drop (fromIntegral binSize) kmer
      Nothing -> return ()
    ) $ B.lines input

  mapM_ hClose $ Map.elems fileHandles
  putStrLn "Done"
