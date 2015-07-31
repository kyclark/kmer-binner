import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  let header = B.pack ">1"
  input <- B.getContents 
  mapM_ (\line -> mapM_ B.putStrLn [header, line]) $ B.lines input
