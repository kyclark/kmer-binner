import System.IO
import System.Environment
import Bio.Sequence.Fasta
import Data.Stringable (toString)

main :: IO ()
main = do
  [f] <- getArgs
  reads <- readFasta f
  let kmers = concat $ map (findKmers 20) $ map (toString . seqdata) reads
  mapM_ printMer kmers
  putStrLn "Done."

findKmers :: Int -> String -> [String]
findKmers k xs = findKmers' n k xs
  where n = length xs - k + 1
        findKmers' n' k' xs'
          | n' > 0 = take k' xs' : findKmers' (n' - 1) k' (tail xs')
          | otherwise = []

printMer :: String -> IO ()
printMer mer = do
  let bin = take 5 mer
  handle <- openFile ("out/" ++ bin) AppendMode
  hPutStrLn handle $ drop 5 mer
  hClose handle
