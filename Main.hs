import ElfParser
import qualified Data.ByteString as B
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Console.GetOpt

data Options = GetSectionHeader | GetHeader | GetProgramHeader

option :: [OptDescr Options]
option = [ Option ['p'] ["program-headers"] (NoArg GetProgramHeader) "List program headers",
           Option ['s'] ["section-headers"] (NoArg GetSectionHeader) "List section headers",
           Option ['e'] ["header"] (NoArg GetHeader) "Shoe the header"]

parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    case parse argv of  
        ([GetProgramHeader], file, []) -> return GetProgramHeader
        ([GetSectionHeader], file, []) -> return GetSectionHeader
        ([GetHeader], file,  []) -> return GetHeader
        (_, _, errs) -> die $ concat errs
  where
    parse = getOpt Permute option
    dump = hPutStrLn stderr
    die errs = dump errs >> exitWith (ExitFailure 1)
    

main :: IO ()
main = do
    opt <- parseArgs
    input <- B.readFile "linker"
    case parseElf parseELFFile input of
        Right value ->
            case opt of
                GetProgramHeader -> putStrLn $ show (elfProgramHeaders value) 
                GetSectionHeader -> putStrLn $ show (elfSectionHeaders value) 
                GetHeader -> putStrLn $ show (elfHeader value) 
        Left d -> putStrLn d
 
