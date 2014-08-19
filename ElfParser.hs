{-|
    Module          : ElfParser
    Description     : ELF Parser
    Copyright       : (c) Mathieu Suen, 2014
    License         : MIT
    Maintainer      : mathk.sue@gmail.com
 -}
module ElfParser
    (
      ELFInfo,
      ELFSection(..),
      ELFHeader(..),
      ELFProgramHeader(..),
      ELFSectionHeader(..),
      sectionHeader, 
      -- * Accessing elf object
      sectionHeaders, programHeaders, header, size, sections,
      sectionFromName, sectionFromIndex, sectionFromHeader,
      sectionName,
      -- * Symbol related function
      symbolAt, symbolName,
      -- * Parsing function
      parseFile, sectionContent,
      parse
    ) where

import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad (forM)
import Control.Monad.State
import Control.Monad.Trans
import qualified FileDecoding as F
import qualified Data.Map as Map
import Text.Printf
import Data.Word
import Data.List
import Data.Ord
import Data.Int (Int64)
import Data.Maybe
import Data.Bits

{- ELF Data type -}
data ELFHeaderMagic = ELFHeaderMagic Word8 String

data ELFHeaderVersion = ELFDefaultVersion | ELFOtherVersion

data ELFHeaderABI = ELFHeaderABI Word8

data ELFHeaderType = ELFRelocatable | ELFExecutable | ELFShared | ELFCore

data ELFProgramHeaderType =
        ELFPHTNull      |
        ELFPHTLoad      |
        ELFPHTDynamic   |
        ELFPHTInterp    |
        ELFPHTNote      |
        ELFPHTShlib     |
        ELFPHTPhdr      |
        ELFPHTTls       |
        ELFPHTLoos      |
        ELFPHTHios      |
        ELFPHTLoProc    |
        ELFPHTHiProc    |
        ELFPHTGnuEhFrame|
        ELFPHTGnuStack  |
        ELFPHTGnuRelro  |
        ELFPHTArmExUnwind

data ELFSectionHeaderType =
       ELFSHTNull       |
       ELFSHTProgBits   |
       ELFSHTSymTab     |
       ELFSHTStrTab     |
       ELFSHTRela       |
       ELFSHTHash       |
       ELFSHTDynamic    |
       ELFSHTNote       |
       ELFSHTNoBits     |
       ELFSHTRel        |
       ELFSHTShlib      |
       ELFSHTDynSym     |
       ELFSHTInitArray  |
       ELFSHTFiniArray  |
       ELFSHTPreinitArray |
       ELFSHTGroup      |
       ELFSHTSymTabShndx|
       ELFSHTLoos       |
       ELFSHTHios       |
       ELFSHTLoProc     |
       ELFSHTArmExIdx   |
       ELFSHTArmPreemptMap |
       ELFSHTArmAttributs |
       ELFSHTArmDebugOverlay |
       ELFSHTArmOverlaySection |
       ELFSHTHiProc     |
       ELFSHTLoUser     |
       ELFSHTHiUser

data ELFHeaderMachine = 
        ELFSPARC    |
        ELFx86      |
        ELFMIPS     |
        ELFPowerPC  |
        ELFARM      |
        ELFSuperH   |
        ELFIA64     |
        ELFx86_64   |
        ELFAArch64

data ELFHeader = ELFHeader {
        magic :: ELFHeaderMagic,
        format :: F.AddressSize,
        fileEndianness :: F.Endianness,
        version :: ELFHeaderVersion,
        osabi :: ELFHeaderABI,
        objectType :: ELFHeaderType,
        machine ::  ELFHeaderMachine,
        entry :: Int64,
        phoff :: Int64,
        shoff :: Int64,
        flags :: Word32,
        hsize :: Word16,
        phentsize :: Word16,
        phnum :: Word16,
        shentsize :: Word16,
        shnum :: Word16,
        shstrndx :: Word16
    }

data ELFProgramHeader = ELFProgramHeader {
        phtype :: ELFProgramHeaderType,
        phoffset :: Int64,
        phvaddr :: Int64,
        phpaddr :: Int64,
        phfilesz :: Int64,
        phmemsz :: Int64,
        phflags :: Int64,
        phalign :: Int64
    }

data ELFSectionHeader = ELFSectionHeader {
        shname :: Word32,
        shtype :: ELFSectionHeaderType,
        shflags :: Int64,
        shaddr :: Int64,
        shoffset :: Int64,
        shsize :: Int64,
        shlink :: Word32,
        shinfo :: Word32,
        shaddralign :: Int64,
        shentrysize :: Int64
    }

data ELFSymbolBind = SbLocal | SbGlobal | SbWeak | SbOsUndefine | SbProcUndefine
data ELFSymbolType = StNoType | StObject | StFunc | StSection | StFile | StCommon | StTls | StOsUndefine | StProcUndefine

data ELFSymbol = ELFSymbol {
        symname :: String,
        symaddr :: Int64,
        symsize :: Int64,
        symbind :: ELFSymbolBind,
        symtype :: ELFSymbolType,
        symother :: Word8,
        symndx  :: Word16
}

data ELFInfo = ELFInfo {
        elfHeader :: ELFHeader,
        elfProgramHeaders :: [ELFProgramHeader],
        elfSectionHeaders :: [ELFSectionHeader],
        elfFileSize :: Int,
        elfSections :: Map.Map Word32 ELFSection
    }


data ParseState = ParseState {
        elfOffset :: Int64,
        elfString :: B.ByteString,
        elfEndianness :: F.Endianness,
        elfSize :: F.AddressSize,
        elfOffsetState :: [Int64]
    }

-- | Represent content of a section
data ELFSection = 
    -- ^ Section of type ELFSHTProgBits
    BinarySection Int64 B.ByteString |
    -- ^ Section of type ELFSHTStrTab
    StringTableSection (Map.Map Word32 String) |
    -- ^ Symbol table
    SymbolTable (Map.Map Int64 ELFSymbol)

type ParseElf a = F.Parse ParseState a

{- Instance declaration -}
instance Show ELFSection where
    show (StringTableSection map) = (show map)
    show (BinarySection off _) = printf "%d: Binary data..." off
    show (SymbolTable list) = (show list)

instance Show ELFHeaderMagic where
    show (ELFHeaderMagic w s) = printf "0x%02X %s" w s

instance Show ELFHeaderVersion where
    show ELFDefaultVersion = "Original"
    show ELFOtherVersion   = "Other"

instance Show ELFHeaderType where
    show ELFRelocatable = "relocatable"
    show ELFExecutable = "executalbe"
    show ELFShared = "shared"
    show ELFCore = "core"

instance Show ELFHeaderMachine where
    show ELFSPARC   = "SPARC"
    show ELFx86     = "x86"
    show ELFMIPS    = "MIPS"
    show ELFPowerPC = "PowerPC"
    show ELFARM     = "ARM"
    show ELFSuperH  = "SuperH"
    show ELFIA64    = "IA-64"
    show ELFx86_64  = "x86-64"
    show ELFAArch64 = "AArch64"

instance Show ELFProgramHeaderType where
    show ELFPHTNull      = "Null Header"
    show ELFPHTLoad      = "Loadable Segment"
    show ELFPHTDynamic   = "Dynamic Linking Information"
    show ELFPHTInterp    = "Interpreter Path"
    show ELFPHTNote      = "Auxiliary Information"
    show ELFPHTShlib     = "Shlib"
    show ELFPHTPhdr      = "Program Header"
    show ELFPHTTls       = "Thread Local Storage"
    show ELFPHTLoos      = "Loos OS specific information"
    show ELFPHTHios      = "Hios OS specific information"
    show ELFPHTLoProc    = "LoProc Processor specific information"
    show ELFPHTHiProc    = "HiProc Processor specific information"   
    show ELFPHTGnuEhFrame= "Exception Handling Information"
    show ELFPHTGnuStack  = "Stack Permissions"
    show ELFPHTGnuRelro  = "Read Only Relocation Segment"
    show ELFPHTArmExUnwind= "Excpetion Unwind Table"

instance Show ELFSectionHeaderType where
    show ELFSHTNull         = "Null Header"
    show ELFSHTProgBits     = "Program Information"
    show ELFSHTSymTab       = "Symbol Table"
    show ELFSHTStrTab       = "String Table"
    show ELFSHTRela         = "Relocation Entries with Addends"
    show ELFSHTHash         = "Hash Table"
    show ELFSHTDynamic      = "Dynamic Linking Information"
    show ELFSHTNote         = "Note Section"
    show ELFSHTNoBits       = "No Bits"
    show ELFSHTRel          = "Relocation Entries without Addends"
    show ELFSHTShlib        = "Reserved"
    show ELFSHTDynSym       = "Symbol Table"
    show ELFSHTInitArray    = "Array of Initialization Functions"
    show ELFSHTFiniArray    = "Array of termination Function"
    show ELFSHTPreinitArray = "Pre initialization Array of Functions"
    show ELFSHTGroup        = "Group Sections"
    show ELFSHTSymTabShndx  = "Array of Symbol Table Index"
    show ELFSHTLoos         = "Loos OS Range"
    show ELFSHTHios         = "Hios OS Range"
    show ELFSHTLoProc       = "LoProc Prcocessor Range"
    show ELFSHTHiProc       = "HiProc Processor Range"
    show ELFSHTArmExIdx     = "Exception Index Table"
    show ELFSHTArmPreemptMap= "BPABI DLL dynamic linking pre-emption map"
    show ELFSHTArmAttributs = "Object file compatibility attributes"
    show ELFSHTArmDebugOverlay = "Debug Overlay"
    show ELFSHTArmOverlaySection = "Overlay Section"
    show ELFSHTLoUser       = "LoUser User Range"
    show ELFSHTHiUser       = "HiUser User Range"

instance Show ELFHeader where
    show ELFHeader { magic=m, format=c, fileEndianness=e, version=v, osabi=abi, objectType=t, machine=arch, entry=ent, phoff=ph, shoff=sh, flags=f, hsize=hs, phentsize=phes, phnum=phn, shentsize=shes, shnum=shn, shstrndx=shsi} =
        printf "Magic: %s\nClass: %s\nF.Endianness: %s\nVersion: %s\nOSABI: %s\nType: %s\nMachine: %s\nEntry point: %s\nPhoff: %s\nShoff: %s\nFlags: 0x%08X\nHeader Size: %d\nProgram Header Size: %d\nProgram Header Entry Number: %d\nSection Header Size: %d\nSection Header Entry Number: %d\nIndex Section Name: %d"
            (show m)
            (show c)
            (show e)
            (show v)
            (show abi)
            (show t)
            (show arch)
            (show ent)
            (show ph)
            (show sh)
            f
            hs
            phes
            phn
            shes
            shn
            shsi

instance Show ELFProgramHeader where 
    show ELFProgramHeader {phtype=pht, phoffset=pho, phvaddr=phv, phpaddr=php, phfilesz=phfs, phmemsz=phm, phflags=phf, phalign=pha} =
        printf "{\nProgram Header Type: %s\nProgram Header Offset: %s\nVirtual Address: %s\nPhysical Address: %s\nSegment File Size: %s\nSegment Memory Size: %s\nFlags: %s\nSegment Alignment: %s\n}"
            (show pht)
            (show pho)
            (show phv)
            (show php)
            (show phfs)
            (show phm)
            (show phf)
            (show pha)

instance Show ELFSectionHeader where
    show ELFSectionHeader {shname=shn, shtype=sht, shflags=shflgs, shaddr=sha, shoffset=sho, shsize=shs, shlink=shl, shinfo=shi, shaddralign=shaa, shentrysize=shes} =
        printf "%s { \nSection Type: %s\nSection Flags: %s\nSection Address: %s\nSection Offset: %s\nSection Size: %s\nSection Link: %s\nSection Info: %s\nSection Address Align: %s\nSection Entry Size: %s\n}"
            (show shn)
            (show sht)
            (show shflgs)
            (show sha)
            (show sho)
            (show shs)
            (show shl)
            (show shi)
            (show shaa)
            (show shes)

instance Show ELFHeaderABI where
    show (ELFHeaderABI abi) = printf "ABI(0x%02X)" abi


instance Show ELFSymbol where
    show (ELFSymbol {symname=name,symndx=ndx,symaddr=addr}) = printf "{%s:\n\tIn section=%d\n\tAddress=%08X\n}" name ndx addr

instance F.ParseStateAccess ParseState where
    offset = elfOffset
    string = elfString
    endianness = elfEndianness
    putOffset a off = a { elfOffset = off }
    pushOffset a@ParseState {elfOffsetState=x} off = a { elfOffset=off, elfOffsetState=(elfOffset a):x}
    popOffset a@ParseState {elfOffsetState=x:xs} = a {elfOffset=x, elfOffsetState=xs} 
    
{-- ELF Manipulation --}

-- | Transform a word to a ELFSymbolBind
wordToSymbolBind :: Word8 -> ELFSymbolBind
wordToSymbolBind 0 = SbLocal
wordToSymbolBind 1 = SbLocal
wordToSymbolBind 2 = SbLocal
wordToSymbolBind w
    | w >= 10 && w <=12 = SbOsUndefine
    | w >= 13 && w <= 15 = SbProcUndefine

-- | Transform a word to a ELFSymbolType
wordToSymbolType :: Word8 -> ELFSymbolType
wordToSymbolType 0 = StNoType
wordToSymbolType 1 = StObject
wordToSymbolType 2 = StFunc
wordToSymbolType 3 = StSection
wordToSymbolType 4 = StFile
wordToSymbolType 5 = StCommon
wordToSymbolType 6 = StTls
wordToSymbolType w 
    | w >= 10 && w <=12 = StOsUndefine
    | w >= 13 && w <= 15 = StProcUndefine

{------------------------------------------------------------------------------
 - Function to search in a ELFInfo data.
 -----------------------------------------------------------------------------}

-- | Get the size in byte of the pars file
fileSize :: ELFInfo -> Int
fileSize ELFInfo {elfFileSize=s} = s

-- | Get the list of header program
programHeaders :: ELFInfo -> [ELFProgramHeader]
programHeaders ELFInfo{elfProgramHeaders=phs} = phs

-- | Get the list of section header
sectionHeaders :: ELFInfo -> [ELFSectionHeader]
sectionHeaders ELFInfo{elfSectionHeaders=shs} = shs

-- | Get the elf header
header :: ELFInfo -> ELFHeader
header ELFInfo{elfHeader=h} = h

sections :: ELFInfo -> Map.Map Word32 ELFSection
sections ELFInfo{elfSections=s} = s

-- | Get the size in byte of the elf file
size :: ELFInfo -> Int
size ELFInfo{elfFileSize=s} = s

-- | Get Section from it section header
sectionFromHeader :: ELFSectionHeader -> ELFInfo -> Maybe ELFSection
sectionFromHeader h info = Map.lookup (shname h) (elfSections info)
 
-- | Get a section base on the index in the section header table 
sectionFromIndex ::  Word16  -> ELFInfo -> Maybe ELFSection
sectionFromIndex index info = sectionFromHeader ((elfSectionHeaders info) !! fromIntegral index) info

-- | Get the section from its name
sectionFromName :: String -> ELFInfo -> Maybe ELFSection
sectionFromName name info = do
    h <- sectionHeader info name 
    sectionFromHeader h info

-- | Retrive the section containing symbol table
symbolTable :: ELFInfo -> Maybe ELFSection
symbolTable info = case sectionFromName ".dynsym" info of
        Just s -> return s
        Nothing -> sectionFromName ".symtab" info

-- | Get the symbol at a specific offset
symbolAt :: ELFInfo -> Int64 -> Maybe ELFSymbol
symbolAt info offset = do
    SymbolTable table <- symbolTable info
    Map.lookup offset table

symbolName :: ELFSymbol -> String
symbolName = symname
    


-- | Get the specific section header
-- 
-- Example usage:
--
-- > sectionHeader elfFile ".text" 
sectionHeader :: ELFInfo -> String -> Maybe ELFSectionHeader
sectionHeader info@ELFInfo {elfSectionHeaders=sh} searchName = find matchName sh
    where matchName sh = maybe False (\sname -> sname == searchName) (sectionName info sh)

stringTableSectionHeader :: ELFInfo -> ELFSectionHeader
stringTableSectionHeader (ELFInfo {elfHeader=h, elfSectionHeaders=shs}) = shs !! (fromIntegral (shstrndx h))

stringFromOffset :: ELFSection -> Word32 -> Maybe String
stringFromOffset (StringTableSection st) offset = do
    (k,v) <- Map.lookupLE offset st
    return $ drop (fromIntegral $ offset - k) v
stringFromOffset _ _ = Nothing

-- | Get the name of a section
sectionName :: ELFInfo -> ELFSectionHeader -> Maybe String
sectionName (info@ELFInfo {elfSections=sections}) (ELFSectionHeader {shname=off}) = do
    s <- Map.lookup (shname $ stringTableSectionHeader info) sections
    stringFromOffset s off

{- ELf specific routine -}
parseHeaderClass :: ParseElf F.AddressSize
parseHeaderClass = do
    b <- F.parseByte
    case b of
        1 -> do
            state <- F.getState
            F.putState state {elfSize = F.S32}
            return F.S32
        2 -> do 
            state <- F.getState
            F.putState state {elfSize = F.S64}
            return F.S64
        _ -> F.bail $ printf "Unknown class (0x02X)" b

parseHeaderMagic :: ParseElf ELFHeaderMagic
parseHeaderMagic = do 
    magicByte <- F.parseByte
    F.assert (magicByte == 0x7F) "First magic byte is wrong"
    ident <- F.parseIdentifier
    F.assert (ident == "ELF") (printf "Magic string is not ELF %s" ident)
    return (ELFHeaderMagic magicByte ident)

parseHeaderEndianness :: ParseElf F.Endianness
parseHeaderEndianness = do
    b <- F.parseByte
    case b of
        1 -> do
            state <- F.getState
            F.putState state {elfEndianness = F.LittleEndian }
            return F.LittleEndian
        0 -> do
            state <- F.getState 
            F.putState state {elfEndianness = F.BigEndian}
            return F.BigEndian
        _ -> F.bail $ printf "Bad endianness (0x%02X)" b

parseHeaderType :: ParseElf ELFHeaderType
parseHeaderType = do
    b <- F.parseHalf
    case b of
        1 -> return ELFRelocatable
        2 -> return ELFExecutable
        3 -> return ELFShared
        4 -> return ELFCore
        _ -> F.bail $ printf "Bad elf type (0x%02X)" b

parseHeaderMachine :: ParseElf ELFHeaderMachine
parseHeaderMachine = do
    b <- F.parseHalf
    case b of
        0x02 -> return ELFSPARC
        0x03 -> return ELFx86
        0x08 -> return ELFMIPS
        0x14 -> return ELFPowerPC
        0x28 -> return ELFARM
        0x2A -> return ELFSuperH
        0x32 -> return ELFIA64
        0x3E -> return ELFx86_64
        0xB7 -> return ELFAArch64
        _ -> F.bail $ printf "Unknown machine (0x%02X)" b

parseHeaderVersion :: ParseElf ELFHeaderVersion
parseHeaderVersion = do 
    b <- F.parseByte
    case b of
        1 -> return ELFDefaultVersion
        _ -> return ELFOtherVersion

parseHeaderABI :: ParseElf ELFHeaderABI
parseHeaderABI = do
    b <- F.parseByte
    return (ELFHeaderABI b)

{-|
    This funcition parse the ELF header extracting all the usefull information
 -}
parseHeader :: ParseElf ELFHeader
parseHeader = do
        m <-    parseHeaderMagic
        f <-    parseHeaderClass
        endian <- parseHeaderEndianness
        v <-    parseHeaderVersion
        abi <-  parseHeaderABI
        F.skip 8
        t <-    parseHeaderType
        arch <- parseHeaderMachine
        F.skip 4
        e <-    parseMachineDepWord
        ph <-   parseMachineDepWord 
        sh <-   parseMachineDepWord 
        flgs <- F.parseWord
        hs <-   F.parseHalf
        phes <- F.parseHalf
        phn <-  F.parseHalf
        shes <- F.parseHalf
        shn <-  F.parseHalf
        shsi <- F.parseHalf
        return ELFHeader {magic=m, format=f, fileEndianness=endian, version=v, osabi=abi, objectType=t, machine=arch, entry=e, phoff=ph, shoff=sh, flags=flgs, hsize=hs, phentsize=phes, phnum=phn, shentsize=shes, shnum=shn, shstrndx=shsi}

-- | Either a 4 byte or 8 byte word 
--   depending on the machine word size.
--   This can be used to parse address and offset
parseMachineDepWord :: ParseElf Int64
parseMachineDepWord = do
    state <- F.getState
    case elfSize state of 
        F.S32 -> fromIntegral <$> F.parseWord
        F.S64 -> fromIntegral <$> F.parseGWord


parseProgramHeaderType :: ParseElf ELFProgramHeaderType
parseProgramHeaderType = do
    w <- F.parseWord
    case w of 
        0 -> return ELFPHTNull
        1 -> return ELFPHTLoad
        2 -> return ELFPHTDynamic
        3 -> return ELFPHTInterp
        4 -> return ELFPHTNote
        5 -> return ELFPHTShlib
        6 -> return ELFPHTPhdr
        7 -> return ELFPHTTls
        0x60000000 -> return ELFPHTLoos
        0x6474e550 -> return ELFPHTGnuEhFrame
        0x6474e551 -> return ELFPHTGnuStack
        0x6474e552 -> return ELFPHTGnuRelro
        0x6FFFFFFF -> return ELFPHTHios
        0x70000000 -> return ELFPHTLoProc
        0x70000001 -> return ELFPHTArmExUnwind 
        0x7FFFFFFF -> return ELFPHTHiProc
        _ -> F.bail $ printf "Unrecognized program header type 0x%08X" w

parseSectionHeaderType :: ParseElf ELFSectionHeaderType
parseSectionHeaderType = do
    w <- F.parseWord
    case w of 
        0 -> return ELFSHTNull
        1 -> return ELFSHTProgBits
        2 -> return ELFSHTSymTab
        3 -> return ELFSHTStrTab
        4 -> return ELFSHTRela
        5 -> return ELFSHTHash
        6 -> return ELFSHTDynamic
        7 -> return ELFSHTNote
        8 -> return ELFSHTNoBits
        9 -> return ELFSHTRel
        10 -> return ELFSHTShlib
        11 -> return ELFSHTDynSym
        14 -> return ELFSHTInitArray
        15 -> return ELFSHTFiniArray
        16 -> return ELFSHTPreinitArray
        17 -> return ELFSHTGroup
        18 -> return ELFSHTSymTabShndx
        0x60000000 -> return ELFSHTLoos
        0x6FFFFFFF -> return ELFSHTHios
        0x70000000 -> return ELFSHTLoProc
        0x70000001 -> return ELFSHTArmExIdx
        0x70000002 -> return ELFSHTArmPreemptMap
        0x70000003 -> return ELFSHTArmAttributs
        0x70000004 -> return ELFSHTArmDebugOverlay
        0x70000005 -> return ELFSHTArmOverlaySection
        0x7FFFFFFF -> return ELFSHTHiProc
        0x80000000 -> return ELFSHTLoUser
        0x8FFFFFFF -> return ELFSHTHiUser
        _ -> F.bail $ printf "Unrecognized section header type 0x%08X" w

parseString :: ParseElf String
parseString = fmap F.w2c <$> F.parseWhile (\w -> not $ w == 0)

parseProgramHeader :: ParseElf ELFProgramHeader
parseProgramHeader = do
    pht <-  parseProgramHeaderType
    pho <-  parseMachineDepWord 
    phv <-  parseMachineDepWord 
    php <-  parseMachineDepWord 
    phfs <- parseMachineDepWord 
    phm <-  parseMachineDepWord 
    phf <-  parseMachineDepWord 
    pha <-  parseMachineDepWord 
    return ELFProgramHeader {phtype=pht, phoffset=pho, phvaddr=phv, phpaddr=php, phfilesz=phfs, phmemsz=phm, phflags=phf, phalign=pha}

parseSectionHeader :: ParseElf ELFSectionHeader
parseSectionHeader = do
    shn <-  F.parseWord
    sht <-  parseSectionHeaderType
    shflgs <- parseMachineDepWord
    sha <-  parseMachineDepWord
    sho <-  parseMachineDepWord 
    shs <-  parseMachineDepWord 
    shl <-  F.parseWord
    shi <-  F.parseWord
    shaa <- parseMachineDepWord 
    shes <- parseMachineDepWord 
    return ELFSectionHeader {shname=shn, shtype=sht, shflags=shflgs, shaddr=sha, shoffset=sho, shsize=shs, shlink=shl, shinfo=shi, shaddralign=shaa, shentrysize=shes}

parseArray :: ParseElf a -> Int -> ParseElf [a]
parseArray parser 0 = return []
parseArray parser n
    | n > 0 = do 
        h <- parser
        (h:) <$> (parseArray parser (n - 1))
    | otherwise = F.bail "Can not parse negative number of array element" 

parseProgramHeaders :: Int -> ParseElf [ELFProgramHeader]
parseProgramHeaders = parseArray parseProgramHeader

parseSectionHeaders :: Int -> ParseElf [ELFSectionHeader]
parseSectionHeaders = parseArray parseSectionHeader

-- | Move to the section that contain the name of all section
moveToStringSectionName :: ELFInfo -> ParseElf ()
moveToStringSectionName (ELFInfo {elfHeader=h, elfSectionHeaders=shs}) =
    F.moveTo $ shoffset (shs !! (fromIntegral (shstrndx h)))

-- | Get the section content given the section name
sectionContent :: ELFInfo -> String -> ParseElf (ELFSectionHeader, B.ByteString)
sectionContent info string = do
    case sectionHeader info string of
        Just h@(ELFSectionHeader {shoffset=off, shsize=size}) -> do
            F.moveTo off
            b <- F.parseRaw size
            return (h,b) 
        Nothing -> F.bail "Section not found"

stringsMapUpTo :: Int64 -> Int64 -> ParseElf (Map.Map Word32 String)
stringsMapUpTo beginOff maxOff = do
    currentOff <- F.offset <$> F.getState
    if currentOff + 1 >= maxOff
    then return (Map.singleton 0 "NullString")
    else do 
        F.moveTo $ currentOff + 1
        Map.insert (fromIntegral (currentOff + 1 - beginOff)) <$> parseString <*> stringsMapUpTo beginOff maxOff

-- | Inner function that recurse over the symbol table to build a map of
-- symbols
symbolTableUpTo :: String -> Int64 -> StateT ELFInfo (F.Parse ParseState) (Map.Map Int64 ELFSymbol)
symbolTableUpTo stringTable maxOffset = do
    currentOff <- lift (F.offset <$> F.getState)
    if currentOff + 1 >= maxOffset
    then return Map.empty
    else do
        sym@(ELFSymbol {symaddr=addr}) <- parseSymbol stringTable 
        Map.insert addr  sym <$> (symbolTableUpTo stringTable maxOffset)

parseStringTable :: ELFSectionHeader -> ParseElf ELFSection
parseStringTable (ELFSectionHeader {shtype=ELFSHTStrTab, shoffset=offset, shsize=size}) = do
    F.moveTo offset
    map <- stringsMapUpTo offset (size+offset)
    return $ StringTableSection map

parseSymbolTable :: ELFSectionHeader -> StateT ELFInfo (F.Parse ParseState) ELFSection
parseSymbolTable (ELFSectionHeader {shtype=ELFSHTSymTab,shoffset=off,shsize=size}) = do
    lift $ F.moveTo off
    SymbolTable <$> (symbolTableUpTo ".strtab" $ off+size)
parseSymbolTable (ELFSectionHeader {shtype=ELFSHTDynSym,shoffset=off,shsize=size}) = do
    lift $ F.moveTo off
    SymbolTable <$> (symbolTableUpTo ".dynstr" $ off+size)

-- | Parse one saymbol entry in a symbol table.
-- TODO: 32 and 64 bit ELF has different way of parsing this structure
parseSymbol :: String -> StateT ELFInfo (F.Parse ParseState) ELFSymbol
parseSymbol stringTable = do
    shnameidx <- lift F.parseWord
    shadd <- lift parseMachineDepWord
    shsize <- lift parseMachineDepWord
    shinfo <- lift F.parseByte
    shother <- lift F.parseByte
    shndx <- lift F.parseHalf
    sectionTable <- sectionFromName stringTable <$> get
    let shbind = wordToSymbolBind $ shinfo `shiftR` 4 
        shtype = wordToSymbolType $ shinfo .&. 0xF
        symbolName 
            | isJust sectionTable = maybe "" id $ stringFromOffset (fromJust sectionTable) shnameidx
            | otherwise = "Null symbol"
        in return $ ELFSymbol symbolName shadd shsize shbind shtype shother shndx

-- | Add a section to the state
addSection :: ELFSectionHeader -> ELFSection -> StateT ELFInfo (F.Parse ParseState) ()
addSection h s = modify (\info@ELFInfo{elfSections=map} -> info {elfSections=Map.insert (shname h) s map})  

-- | Set the name of the section
setSectionName :: StateT ELFInfo (F.Parse ParseState) ()
setSectionName = do
    info@ELFInfo{elfSections=s} <- get
    section <- lift $ parseStringTable (stringTableSectionHeader info)
    put $ info {elfSections=Map.insert ((shname . stringTableSectionHeader) info) section s}

-- | Set the symbol table in the ELFInfo structure
setSymbolTable :: String -> StateT ELFInfo (F.Parse ParseState) ()
setSymbolTable sectionName = do
    info <-get
    case sectionHeader info sectionName of
        Just h -> do
            section <- parseSymbolTable h
            addSection h section
        Nothing -> return ()

-- | Get the .text section and store it into the ELFInfo
setTextSection :: StateT ELFInfo (F.Parse ParseState) ()
setTextSection = do
    info@ELFInfo{elfSections=s} <- get
    (ELFSectionHeader {shoffset=o,shname=n},b) <- lift (sectionContent info ".text")
    put $ info {elfSections=(Map.insert n (BinarySection o b) s)}

-- | Parse a string table and add it to the sections.
setStringTable :: String -> StateT ELFInfo (F.Parse ParseState) ()
setStringTable sectionName = do
    info <- get
    case sectionHeader info sectionName of
        Just h ->  do
            section <- lift $ parseStringTable h
            addSection h section
        Nothing -> return ()

-- | Post initialise the info data
annotateELFInfo :: StateT ELFInfo (F.Parse ParseState) ()
annotateELFInfo = do
    setSectionName
    setStringTable ".strtab"
    setStringTable ".dynstr"
    setSymbolTable ".dynsym"
    setTextSection
    
-- | Parse an ELF file.
-- This function first parse the different header in the ParseElf monad
-- end then continue using the transformer State monad.
parseFile :: ParseElf ELFInfo
parseFile = do
    hdr <- parseHeader
    F.moveTo $ phoff hdr
    phs <- parseProgramHeaders $ fromIntegral (phnum hdr)
    F.moveTo $ shoff hdr
    shs <- parseSectionHeaders $ fromIntegral (shnum hdr)
    state <- F.getState
    execStateT annotateELFInfo ELFInfo {elfHeader=hdr, elfProgramHeaders=phs, elfSectionHeaders=shs, elfFileSize=(fromIntegral $ B.length $ F.string state), elfSections=Map.empty }

parse :: ParseElf a -> B.ByteString -> Either String a 
parse parser string = F.parse ParseState {elfOffset=0, elfSize=F.S32, elfEndianness=F.LittleEndian, elfString=string, elfOffsetState=[] } parser string

