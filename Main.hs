import FileDecoding
import ElfParser
import qualified Data.ByteString as B

main :: IO ()
main = do
    input <- B.readFile "linker"
    case parse parseELFHeader  input (ELFParserState { size = S32 }) of
        Right value -> print value
        Left d -> putStrLn d
 
