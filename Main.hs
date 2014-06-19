import ElfParser
import qualified Data.ByteString as B

main :: IO ()
main = do
    input <- B.readFile "linker"
    case parseElf parseELFHeader input of
        Right value -> print value
        Left d -> putStrLn d
 
