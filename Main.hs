import ElfParser
import qualified Data.ByteString as B
import qualified Graphics.UI.Threepenny as UI
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Console.GetOpt
import Graphics.UI.Threepenny.Core
import Control.Monad

getStaticDir :: IO FilePath
getStaticDir = return "./wwwroot/"

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
    static <- getStaticDir
    startGUI defaultConfig { tpStatic = Just static } setup

setup :: Window -> UI ()
setup w = do
    return w # set UI.title "ELF Parser"
    input <- liftIO $ B.readFile "linker"
    case parseElf parseELFFile input of
        Right value -> do
            getBody w #+ ((UI.h1 # set UI.text "ELF Header") : displayElfHeader (elfHeader value))
            return ()
        Left d -> do
            getBody w #+ [UI.h1 # set UI.text ("Error while parsing: " ++ d)]
            return ()


displayElfHeader :: ELFHeader -> [UI Element]
displayElfHeader ELFHeader { 
        magic=m, 
        format=c, 
        fileEndianness=e, 
        version=v, 
        osabi=abi, 
        objectType=t, 
        machine=arch, 
        entry=ent, 
        phoff=ph, 
        shoff=sh, 
        flags=f, 
        hsize=hs, 
        phentsize=phes, 
        phnum=phn, 
        shentsize=shes, 
        shnum=shn, 
        shstrndx=shsi} = do
    [UI.dlist #+ [
        UI.ddef # set UI.text "e_ident",
        UI.dterm # set UI.text (show m),
        UI.li # set UI.text (show m)]]
     

{-main :: IO ()
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
-}
 
