import FileDecoding
import qualified Data.ByteString as B

main :: IO ()
main = do
    input <- B.readFile "linker"
    case parse parseELFHeader  input of
        Right value -> putStrLn $ show value
        Left d -> putStrLn d
 
