import qualified ElfParser as ELF
import qualified Data.ByteString as B
import qualified Graphics.UI.Threepenny as UI
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Console.GetOpt
import Graphics.UI.Threepenny.Core
import Control.Monad

getStaticDir :: IO FilePath
getStaticDir = return "./"

data Options = GetSectionHeader | GetHeader | GetProgramHeader | GetHelp | StartGui

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
        ([], _, _) -> return StartGui 
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
    case opt of
        StartGui -> do
            static <- getStaticDir
            startGUI defaultConfig {tpStatic=Just static} setup
        _ -> case ELF.parse ELF.parseFile input of
                Right value ->
                    case opt of
                        GetProgramHeader -> putStrLn $ show (ELF.elfProgramHeaders value) 
                        GetSectionHeader -> putStrLn $ show (ELF.elfSectionHeaders value) 
                        GetHeader -> putStrLn $ show (ELF.elfHeader value) 
                Left d -> putStrLn d

setup :: Window -> UI ()
setup w = do
    return w # set UI.title "ELF Parser"
    UI.addStyleSheet w "style.css"
    input <- liftIO $ B.readFile "linker"
    case ELF.parse ELF.parseFile input of
        Right value -> do
            getBody w #+ ((UI.h1 # set UI.text "ELF Header") : displayElfHeader (ELF.elfHeader value))
            return ()
        Left d -> do
            getBody w #+ [UI.h1 # set UI.text ("Error while parsing: " ++ d)]
            return ()


displayElfHeader :: ELF.ELFHeader -> [UI Element]
displayElfHeader ELF.ELFHeader { 
        ELF.magic=m, 
        ELF.format=c, 
        ELF.fileEndianness=e, 
        ELF.version=v, 
        ELF.osabi=abi, 
        ELF.objectType=t, 
        ELF.machine=arch, 
        ELF.entry=ent, 
        ELF.phoff=ph, 
        ELF.shoff=sh, 
        ELF.flags=f, 
        ELF.hsize=hs, 
        ELF.phentsize=phes, 
        ELF.phnum=phn, 
        ELF.shentsize=shes, 
        ELF.shnum=shn, 
        ELF.shstrndx=shsi} = do
    [UI.dlist #+ [
        UI.ddef # set UI.text "e_ident",
        UI.dterm # set UI.text (show m),
        UI.li # set UI.text (show m)]]
     
 
