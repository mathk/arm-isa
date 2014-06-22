import ElfParser
import qualified Data.ByteString as B
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Console.GetOpt

data Options = GetSectionHeader | GetHeader | GetProgramHeader | GetHelp

option :: [OptDescr Options]
option = [ Option ['p'] ["program-headers"] (NoArg GetProgramHeader) "List program headers",
           Option ['s'] ["section-headers"] (NoArg GetSectionHeader) "List section headers",
           Option ['e'] ["header"] (NoArg GetHeader) "Show the elf header",
           Option ['h'] ["help"] (NoArg GetHelp) "Show this message"]

parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    name <- getProgName
    case parse argv of  
        ([GetProgramHeader], file, []) -> return GetProgramHeader
        ([GetSectionHeader], file, []) -> return GetSectionHeader
        ([GetHeader], file,  []) -> return GetHeader
        ([GetHelp], _, _) -> help name
        (_, _, errs) -> die errs name
  where
    parse           = getOpt Permute option
    header name     = "Usage: " ++ name ++ " [-hpse]"
    info name       = usageInfo (header name) option
    dump            = hPutStrLn stderr
    die errs name   = dump (concat errs ++ (info name)) >> exitWith (ExitFailure 1)
    help name       = dump (info name) >> exitWith ExitSuccess
    

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
 
