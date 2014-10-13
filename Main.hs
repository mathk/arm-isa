import qualified ElfParser as ELF
import qualified Data.ByteString as B
import Arm.ArmType
import Arm.Core
import qualified Graphics.UI.Threepenny as UI
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Console.GetOpt
import Graphics.UI.Threepenny.Core
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Data.IORef
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import Text.Printf
import Data.Int (Int64)

getStaticDir :: IO FilePath
getStaticDir = return "./"

data Options = GetSectionHeader | GetHeader | GetProgramHeader | GetSections | GetHelp | StartGui

option :: [OptDescr Options]
option = [ Option ['p'] ["program-headers"] (NoArg GetProgramHeader) "List program headers",
           Option ['s'] ["section-headers"] (NoArg GetSectionHeader) "List section headers",
           Option ['S'] ["sections"] (NoArg GetSections) "List sections",
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
        ([GetSections], file,  []) -> return GetSections
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
                        GetProgramHeader -> putStrLn $ show (ELF.programHeaders value) 
                        GetSectionHeader -> putStrLn $ show (ELF.sectionHeaders value) 
                        GetHeader -> putStrLn $ show (ELF.header value) 
                        GetSections -> putStrLn $ show (ELF.sections value)
                Left d -> putStrLn d

{--
 - Make an element draggable
 --}
draggable :: Element -> UI ()
draggable e = runFunction $ ffi "jsPlumb.getInstance().draggable($(%1))" e

{--
 - Connect ui element
 --}
connect :: Element -> Element  -> UI ()
connect s t = runFunction $ ffi "jsPlumb.getInstance().connect({source:$(%1), target: $(%2)})" s t

{--
 - Add all the javascript required
 --}
setupJavascript :: Window -> UI Element
setupJavascript w = do
        js1 <- UI.mkElement "script"  # 
                set (UI.attr "src") "/static/js/jquery-ui.js" #
                set (UI.attr "type") "text/javascript"
        js2 <- UI.mkElement "script"  # 
                set (UI.attr "src") "/static/js/jquery.jsPlumb-1.6.4-min.js" #
                set (UI.attr "type") "text/javascript"
        getHead w #+ [element js1, element js2]


setup :: Window -> UI ()
setup w = do
    return w # set UI.title "ELF Parser"
    UI.addStyleSheet w "style.css"
    input <- liftIO $ B.readFile "linker"
    case ELF.parse ELF.parseFile input of
        Right value -> do
            let Just (ELF.BinarySection sectionOffset stream) = ELF.sectionFromName ".text" value
            setupJavascript w
            div <- UI.div
            body <- getBody w
            element body #+ [element div]
            canvas <- UI.div #. "asm-block"
            {--on (domEvent "resize") body $ \_ -> do 
                width <- body # get elementWidth
                height <- body # get elementHeight
                element div # set text (printf "(%d,%d)" width height)--}
            element body #+ ((UI.h1 # set UI.text "ELF Header") : (displayElfHeader (ELF.header value) ++ [element canvas]) {-++ [displayElfCanvas value] -})
            runReaderT (displayElfTextSection (parseArmBlock 0 stream)) (canvas,value)
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

type BlockGraph = ReaderT (Element,ELF.ELFInfo) UI

askElement :: BlockGraph Element
askElement = do 
    (e,_) <- ask
    return e

askInfo :: BlockGraph ELF.ELFInfo
askInfo =  do
    (_,i) <- ask
    return i

makeNextBlockButton :: Int64 -> BlockGraph Element
makeNextBlockButton offset = do
    info <- askInfo
    let Just (ELF.BinarySection sectionOffset stream) = ELF.sectionFromName ".text" info
    w <- askElement
    buttonArm <- lift $ UI.button #. "button" #+ [string $ printf "Next Arm at: %d" offset]
    buttonThumb <-lift $ UI.button #. "button" #+ [string $ printf "Next Thumb at: %d" offset]
    lift $ (on UI.click buttonArm $ \_ -> do 
        runReaderT (displayElfTextSection $ parseArmBlock offset stream) (w,info))
    lift $ (on UI.click buttonThumb $ \_ -> do 
        runReaderT (displayElfTextSection $ parseThumbBlock offset stream) (w,info))
    lift $ UI.div #+ [element buttonArm, UI.br, element buttonThumb]
    

displayElfTextSection :: ArmBlock -> BlockGraph ()
displayElfTextSection block = do
    info <- askInfo
    let offset = offsetBlock block
    let Just (ELF.BinarySection sectionOffset stream) = ELF.sectionFromName ".text" info
    let instructions = instructionsBlock block
    buttonNext <- sequence $ map makeNextBlockButton (nextBlocks block)
    title <- lift $ UI.h4 # set UI.text (printf "Block at offset: %08X" (offset+sectionOffset))
    listInstruction <- sequence $ map (displayInstruction (offset + sectionOffset)) instructions
    displayBlock <- lift $ UI.div #+ listInstruction
    body <- askElement
    gridElem <- lift $ grid [[element title], [element displayBlock], fmap element buttonNext] # set (UI.attr "id") (printf "block%d" offset)
    lift $ element gridElem # set UI.draggable True
    {-- lift $ draggable gridElem --}
    lift $ element body #+ [element gridElem]
    return ()

displayInstruction :: Int64 -> ArmInstr -> BlockGraph (UI Element)
displayInstruction sectionOffset inst = do
    info <- askInfo
    case ELF.symbolAt info $ (instructionBlockOffset inst) + sectionOffset of
        Just s -> return $ UI.p #+ [string (ELF.symbolName s), string ":", UI.br, string (show inst)]
        Nothing -> return $ UI.p # set UI.text (show inst)
    
{------------------------------------------------------------------------------
 - Drawing  canvas with all the different section
 -----------------------------------------------------------------------------}
{-- Using a canvas modified version of threepenny


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

normalizeY :: Int64 -> Int -> Double
normalizeY value max= (((fromIntegral value) * 1640.0) / (fromIntegral max)) 

regionSeparator :: Int64 -> Int -> UI.DrawingPath
regionSeparator offset size = UI.line (0.0,offsetCoord) (300.0,offsetCoord)
    where offsetCoord = normalizeY offset size

programSectionOffset :: Int -> ELF.ELFProgramHeader -> UI.Drawing
programSectionOffset size (ELF.ELFProgramHeader {ELF.phoffset=off}) =
    (UI.openedPath red 2.0 (regionSeparator off size))
        where red = UI.solidColor $ UI.rgbColor 0xFF 0 0

sectionOffset :: Int -> ELF.ELFSectionHeader -> UI.Drawing
sectionOffset size (ELF.ELFSectionHeader {ELF.shoffset=off, ELF.shname=sectionName}) =
    (UI.openedPath blue 2.0 $ regionSeparator off size) <>
    (UI.setDraw UI.textFont "bold 16px sans-serif") <>
    (UI.setDraw UI.strokeStyle  black) <>
    (UI.fillText (show sectionName) (300.0, (normalizeY off size)))
        where 
            blue = UI.solidColor $ UI.rgbColor 0x50 0x50 0xFF
            black = UI.solidColor $ UI.rgbColor 0 0 0

initialOffsetMap :: [ELF.ELFSectionHeader] -> Int -> Map.Map Double Double
initialOffsetMap [] _ = Map.empty
initialOffsetMap ((ELF.ELFSectionHeader {ELF.shoffset=off}):xs) maxOffset = Map.insert initialPostion initialPostion (initialOffsetMap xs maxOffset)
    where
        initialPostion = normalizeY off maxOffset 

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
    | keyIndex == mapSize - 1       = (valueAt $ mapSize - 1    , (normalizeY (fromIntegral max) max))
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
            position = normalizeY off size
            textPosition = layoutMap Map.! position
            blue = UI.solidColor $ UI.rgbColor 0x50 0x50 0xFF
            black = UI.solidColor $ UI.rgbColor 0 0 0

drawSectionHeaders :: [ELF.ELFSectionHeader] -> Int -> Map.Map Double Double -> UI.Drawing
drawSectionHeaders [] _ _ = mempty
drawSectionHeaders (h:xs) size layoutMap = (drawSectionHeader h size layoutMap) <> (drawSectionHeaders xs size layoutMap) 

layoutAndDrawSectionHeaders :: [ELF.ELFSectionHeader] -> Int -> UI.Drawing
layoutAndDrawSectionHeaders headers maxSize = drawSectionHeaders headers maxSize (layoutSectionName headers maxSize)

displayElfHeaderOffset :: ELF.ELFInfo -> UI.Drawing
displayElfHeaderOffset info = 
    --(mconcat (fmap (sectionOffset size) sh )) <>
    (layoutAndDrawSectionHeaders shs size) <>
    (mconcat (fmap (programSectionOffset size) phs ))
        where
            shs = ELF.sectionHeaders info
            phs = ELF.programHeaders info
            size = ELF.size info
            green = UI.solidColor $ UI.rgbColor 0x20 0xFF 0
            red = UI.solidColor $ UI.rgbColor 0xFF 0 0

    
--}
