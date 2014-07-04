import qualified ElfParser as ELF
import qualified Data.ByteString as B
import qualified Graphics.UI.Threepenny as UI
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Console.GetOpt
import Graphics.UI.Threepenny.Core
import Control.Monad
import Data.Monoid
import qualified Control.Monad.State as S
import Text.Printf

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
            getBody w #+ ((UI.h1 # set UI.text "ELF Header") : (displayElfHeader (ELF.elfHeader value)) ++ [displayElfCanvas value])
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
        ELF.shstrndx=shsi} =
    [UI.dlist #+ [
        UI.ddef # set UI.text "e_ident",
        UI.dterm # set UI.text (printf "%s, %s, %s, %s, %s"  (show m) (show c) (show e) (show v) (show abi)),
        UI.ddef # set UI.text "e_type",
        UI.dterm # set UI.text (show t),
        UI.ddef # set UI.text "e_machine",
        UI.dterm # set UI.text (show arch),
        UI.ddef # set UI.text "e_entry",
        UI.dterm # set UI.text (show ent),
        UI.ddef # set UI.text "e_phoff",
        UI.dterm # set UI.text (show ph),
        UI.ddef # set UI.text "e_shoff",
        UI.dterm # set UI.text (show sh),
        UI.ddef # set UI.text "e_flags",
        UI.dterm # set UI.text (show f),
        UI.ddef # set UI.text "e_ehsize",
        UI.dterm # set UI.text (show hs),
        UI.ddef # set UI.text "e_phentsize",
        UI.dterm # set UI.text (show phes),
        UI.ddef # set UI.text "e_phnum",
        UI.dterm # set UI.text (show phn),
        UI.ddef # set UI.text "e_shentsize",
        UI.dterm # set UI.text (show shes),
        UI.ddef # set UI.text "e_shnum",
        UI.dterm # set UI.text (show shn),
        UI.ddef # set UI.text "e_shstrndx",
        UI.dterm # set UI.text (show shsi)]]


displayElfCanvas :: ELF.ELFInfo -> UI Element
displayElfCanvas info = do
    canvas <- UI.canvas #
                    set UI.height 800 # 
                    set UI.width 300 # 
                    set style [("border", "solid black 1px")]
    UI.renderDrawing canvas 
        ((UI.openedPath red 4.0 
            (UI.line (10.0,10.0) (100.0,100.0) <>
            (UI.move (150.0,100.0)) <>
            (UI.bezierCurve [(180.0,30.0), (250.0,180.0), (300.0,100.0)]))) <>
        (UI.openedPath red 4.0 (UI.arc (125.0, 115.0) 30.0 0.0 360.0)))
    element canvas
        where red = UI.solidColor (UI.RGB 0xFF 0 0) 
