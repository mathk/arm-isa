import qualified ElfParser as ELF
import qualified ArmDecode as Arm
import qualified ThumbDecode as Thumb
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
import qualified Data.Map as Map
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
            getBody w #+ ((UI.h1 # set UI.text "ELF Header") : (displayElfHeader (ELF.elfHeader value)) ++ [displayElfCanvas value] ++ (displayElfTextSection value input))
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
                    set UI.height 1700 # 
                    set UI.width 600 # 
                    set style [("border", "solid black 1px")]
    UI.renderDrawing canvas (
        (UI.translate 0.0 50.0) <> 
        --(UI.scale 300.0 (-1600.0)) <> 
        (displayElfHeaderOffset info))
        --(UI.openedPath red 0.001
        --    ((UI.translate 150.0 1600.0) <>
        --    (UI.scale 150.0 (-1600.0)) <>
        --    (displayElfHeaderOffset info)) )
        --    (UI.line (-1.0,0.9) (1.0,0.9))))
            --(UI.move (150.0,100.0)) <>
            --(UI.bezierCurve [(180.0,30.0), (250.0,180.0), (300.0,100.0)]))) <>
        -- (UI.openedPath red 4.0 (UI.arc (125.0, 115.0) 30.0 0.0 360.0)))
    element canvas

displayElfTextSection :: ELF.ELFInfo -> B.ByteString -> [UI Element]
displayElfTextSection info s =
    case ELF.parse (ELF.getSectionContent info ".text") s of
        Right stream -> map toUi (Thumb.parseStream stream)
      where toUi armInst = UI.p # set UI.text (show armInst)

normalizeY :: Int -> Int -> Double
normalizeY value max= (((fromIntegral value) * 1640.0) / (fromIntegral max)) 

regionSeparator :: Int -> Int -> UI.DrawingPath
regionSeparator offset size = UI.line (0.0,offsetCoord) (300.0,offsetCoord)
    where offsetCoord = normalizeY offset size

programSectionOffset :: Int -> ELF.ELFProgramHeader -> UI.Drawing
programSectionOffset size (ELF.ELFProgramHeader {ELF.phoffset=off}) =
    (UI.openedPath red 2.0 (regionSeparator (ELF.offsetToInt off) size))
        where red = UI.solidColor $ UI.rgbColor 0xFF 0 0

sectionOffset :: Int -> ELF.ELFSectionHeader -> UI.Drawing
sectionOffset size (ELF.ELFSectionHeader {ELF.shoffset=off, ELF.shname=sectionName}) =
    (UI.openedPath blue 2.0 (regionSeparator (ELF.offsetToInt off) size)) <>
    (UI.setDraw UI.textFont "bold 16px sans-serif") <>
    (UI.setDraw UI.strokeStyle  black) <>
    (UI.fillText (show sectionName) (300.0, (normalizeY (ELF.offsetToInt off) size)))
        where 
            blue = UI.solidColor $ UI.rgbColor 0x50 0x50 0xFF
            black = UI.solidColor $ UI.rgbColor 0 0 0

initialOffsetMap :: [ELF.ELFSectionHeader] -> Int -> Map.Map Double Double
initialOffsetMap [] _ = Map.empty
initialOffsetMap ((ELF.ELFSectionHeader {ELF.shoffset=off}):xs) maxOffset = Map.insert initialPostion initialPostion (initialOffsetMap xs maxOffset)
    where
        initialPostion = (normalizeY (ELF.offsetToInt off) maxOffset) 


sign :: Double -> Double
sign a | a >= 0.0 = 1.0
       | a < 0.0 = -1.0

repulseForce :: Double -> Double -> Double -> Double
repulseForce origin distant preferedSign
    | abs d > 100.0 = 0
    | d < 0 = (-(100.0 + d)) / 15.0
    | otherwise = (100.0 - d) / 15.0
    where d = origin - distant
          

simbling :: Map.Map Double Double -> Double -> Map.Map Double Double
simbling mapOffset key = Map.filterWithKey checkRange mapOffset
    where checkRange filterKey value = and [ key /= filterKey, (abs $ (mapOffset Map.! key) - value) < 100.0 ]

forceAt :: Map.Map Double Double -> Double -> Double
forceAt mapOffset key = (Map.foldl sumingForce 0.0 (simbling mapOffset key)) + ((key - origin) / 200.0)
    where origin = (mapOffset Map.! key)
          preferedSign = sign (key - origin)
          sumingForce acc value =  (repulseForce origin value preferedSign ) + acc

constrainForceAt :: Map.Map Double Double -> Int -> Double -> Double
constrainForceAt mapOffset maxValue key
    | left < newValue && newValue < right = force 
    | newValue < left                       = min 0.0 (left +  0.000001 - oldValue)
    | otherwise                             = max 0.0 (right - 0.000001 - oldValue) 
    where (left,right) = neighbour key mapOffset maxValue
          force = forceAt mapOffset key
          oldValue = (mapOffset Map.! key) 
          newValue = oldValue + force

neighbour :: Double -> Map.Map Double Double -> Int -> (Double,Double)
neighbour key mapOffset max
    | mapSize == 1                  = (0.0                      , valueAt 0)
    | keyIndex == 0 && mapSize > 1  = (0.0                      , valueAt 1)
    | keyIndex == mapSize - 1       = (valueAt $ mapSize - 1    , (normalizeY max max))
    | otherwise                     = (valueAt $ keyIndex - 1   , min 1640.0 (valueAt $ keyIndex + 1))
    where keyIndex = Map.findIndex key mapOffset
          mapSize = Map.size mapOffset
          valueAt index = snd $ Map.elemAt index mapOffset

forceBaseStep :: Map.Map Double Double -> Int -> Map.Map Double Double
forceBaseStep map max = foldl tranform map (Map.keys map)
    where tranform currentMap key = Map.update (Just . ((constrainForceAt currentMap max key)+)) key currentMap 

forceBaseLayout :: Map.Map Double Double -> Int -> Int -> Map.Map Double Double
forceBaseLayout mapOffset _ 0 = mapOffset
forceBaseLayout mapOffset max n = forceBaseLayout (forceBaseStep mapOffset max) max (n - 1)

layoutSectionName :: [ELF.ELFSectionHeader] -> Int -> Map.Map Double Double
layoutSectionName headers max = forceBaseLayout (initialOffsetMap headers max) max 450 

drawSectionHeader :: ELF.ELFSectionHeader -> Int -> Map.Map Double Double -> UI.Drawing
drawSectionHeader (ELF.ELFSectionHeader {ELF.shname=sectionName,ELF.shoffset=off}) size layoutMap = 
    (UI.openedPath blue 2.0 (UI.line (0.0,position) (300.0,position))) <>
    (UI.setDraw UI.textFont "bold 16px sans-serif") <>
    (UI.setDraw UI.strokeStyle  black) <>
    (UI.openedPath blue 2.0 (UI.line (300.0,position) (380.0,textPosition))) <>
    (UI.fillText (printf "%s (%.02g)" (show sectionName) textPosition) (380.0,textPosition))
        where 
            position = normalizeY (ELF.offsetToInt off) size
            textPosition = layoutMap Map.! position
            blue = UI.solidColor $ UI.rgbColor 0x50 0x50 0xFF
            black = UI.solidColor $ UI.rgbColor 0 0 0

drawSectionHeaders :: [ELF.ELFSectionHeader] -> Int -> Map.Map Double Double -> UI.Drawing
drawSectionHeaders [] _ _ = mempty
drawSectionHeaders (h:xs) size layoutMap = (drawSectionHeader h size layoutMap) <> (drawSectionHeaders xs size layoutMap) 

layoutAndDrawSectionHeaders :: [ELF.ELFSectionHeader] -> Int -> UI.Drawing
layoutAndDrawSectionHeaders headers maxSize = drawSectionHeaders headers maxSize (layoutSectionName headers maxSize)

displayElfHeaderOffset :: ELF.ELFInfo -> UI.Drawing
displayElfHeaderOffset (ELF.ELFInfo header ph sh size) = 
    --(mconcat (fmap (sectionOffset size) sh )) <>
    (layoutAndDrawSectionHeaders sh size) <>
    (mconcat (fmap (programSectionOffset size) ph ))
        where
            green = UI.solidColor $ UI.rgbColor 0x20 0xFF 0
            red = UI.solidColor $ UI.rgbColor 0xFF 0 0
 
    
