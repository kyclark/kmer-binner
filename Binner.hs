import Bio.Core.Sequence
import Bio.Sequence.Fasta
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap as Map
import qualified Data.Map as DM
import System.IO
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  reads <- readFasta f
--  reads <- readFasta "test.fa"
  let kmers = concatMap (findKmers 20 . unSD . seqdata) reads
  let f = B.take 5
  let binned      = DM.fromListWith (++) [(f k, [k]) | k <- kmers]
  let allMers     = replicateM 5 "ACTG"
  let fileHandles = Map.fromList $ map (\x -> (x, openFile ("out/" ++ x) WriteMode)) allMers 

  --mapM_ printMer kmers 

  mapM_ (\bin -> do
    let fh   = Map.lookup (B.unpack bin) fileHandles
    let mers = DM.lookup bin binned

    case fh of
      Nothing -> putStrLn ("Missing handle for " ++ (B.unpack bin))
      Just fh -> do
        h <- fh
        case mers of 
          Nothing -> putStrLn ("No mers for " ++ (B.unpack bin))
          Just m  -> mapM_ (B.hPutStrLn h) m
    ) $ DM.keys binned

  --mapM_ (printMer fileHandles) kmers 
  mapM_ (\ioh -> do { h <- ioh; hClose h }) $ Map.elems fileHandles
  putStrLn "Done."

--  let binned = Map.fromList $ map (\k -> (B.take 5 k, k)) kmers
--  print binned
--  let kmers       = concatMap (findKmersBS 20 . seqdata) reads

-- # --------------------------------------------------
findKmers :: Integer -> B.ByteString -> [B.ByteString]
findKmers k xs = findKmers' n k xs
  where n = toInteger (B.length xs) - k + 1
        findKmers' n' k' xs'
          | n' > 0 = B.take (fromIntegral k') xs'
             : findKmers' (n' - 1) k' (B.tail xs')
          | otherwise = []

-- # --------------------------------------------------
--printMer mer = do
--  let bin = toString $ B.take 5 mer
--  B.appendFile ("out/" ++ bin) (B.drop 5 mer)

-- # --------------------------------------------------
--printMer :: Maybe IO Handle -> [Maybe B.ByteString] -> IO ()
--printMer fh mers = do
--  case handle of 
--    Nothing  -> putStrLn $ "Missing " ++ bin ++ " handle"
--    Just ioh -> do h <- ioh
--                   B.hPutStrLn h (B.drop 5 mer)
