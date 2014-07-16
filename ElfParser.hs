{-|
    Module          : ElfParser
    Description     : ELF Parser
    Copyright       : (c) Mathieu Suen, 2014
    License         : MIT
    Maintainer      : mathk.sue@gmail.com
 -}
module ElfParser
    (
      ELFInfo(..),
      ELFHeader(..),
      ELFProgramHeader(..),
      ELFSectionHeader(..),
      sectionHeader, programHeaders, sectionName,
      parseHeader,
      parseFile,
      addressToInt,
      offsetToInt,
      parse
    ) where

import qualified Data.ByteString as B
import Control.Applicative
import qualified FileDecoding as F
import Text.Printf
import Data.Word
import Data.List

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
        entry :: Address,
        phoff :: Address,
        shoff :: Address,
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
        phoffset :: Offset,
        phvaddr :: Address,
        phpaddr :: Address,
        phfilesz :: MachineInt,
        phmemsz :: MachineInt,
        phflags :: MachineInt,
        phalign :: MachineInt
    }

data ELFSectionName = ELFSectionName (Either String Word32)

data ELFSectionHeader = ELFSectionHeader {
        shname :: ELFSectionName,
        shtype :: ELFSectionHeaderType,
        shflags :: MachineInt,
        shaddr :: Address,
        shoffset :: Offset,
        shsize :: MachineInt,
        shlink :: Word32,
        shinfo :: Word32,
        shaddralign :: MachineInt,
        shentrysize :: MachineInt
    }

data ELFInfo = ELFInfo {
        elfHeader :: ELFHeader,
        elfProgramHeaders :: [ELFProgramHeader],
        elfSectionHeaders :: [ELFSectionHeader],
        elfFileSize :: Int
    } deriving (Show)

data ParseState = ParseState {
        elfOffset :: Int,
        elfString :: B.ByteString,
        elfEndianness :: F.Endianness,
        elfSize :: F.AddressSize,
        elfOffsetState :: [Int]
    }

newtype Address = Address (Either Word32 Word64)
newtype Offset = Offset (Either Word32 Word64)
newtype MachineInt = MachineInt (Either Word32 Word64)
type ParseElf a = F.Parse ParseState a

{- Instance declaration -}
instance Show Address where
    show (Address (Left w)) = printf "0x%08X" w
    show (Address (Right w)) = printf "0x%016X" w

instance Show Offset where
    show (Offset (Left w)) = printf "%d" w
    show (Offset (Right w)) = printf "%d" w

instance Show MachineInt where 
    show (MachineInt (Left w)) = printf "%d" w
    show (MachineInt (Right w)) = printf "%d" w

instance Show ELFHeaderMagic where
    show (ELFHeaderMagic w s) = printf "0x%02X %s" w s

instance Show ELFSectionName where
    show (ELFSectionName (Left s)) = s
    show (ELFSectionName (Right s)) = (show s)

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


instance F.ParseStateAccess ParseState where
    offset = elfOffset
    string = elfString
    endianness = elfEndianness
    putOffset a off = a { elfOffset = off }
    pushOffset a@ParseState {elfOffsetState=x} off = a { elfOffset=off, elfOffsetState=(elfOffset a):x}
    popOffset a@ParseState {elfOffsetState=x:xs} = a {elfOffset=x, elfOffsetState=xs} 
    

{-- ELF Manipulation --}
{-- | Get the size in byte of the pars file --}
fileSize :: ELFInfo -> Int
fileSize ELFInfo {elfFileSize=s} = s

{-- | Get the list of header program --}
programHeaders :: ELFInfo -> [ELFProgramHeader]
programHeaders ELFInfo{elfProgramHeaders=phs} = phs

-- | Get the specific section header
-- 
-- Example usage:
--
-- > sectionHeader elfFile ".text" 
sectionHeader :: ELFInfo -> String -> Maybe ELFSectionHeader
sectionHeader ELFInfo {elfSectionHeaders=sh} searchName = find matchName sh
    where matchName ELFSectionHeader {shname=ELFSectionName (Left name)} = name == searchName
          matchName _ = False

-- | Get the name of a section
sectionName :: ELFSectionHeader -> String
sectionName ELFSectionHeader {shname=ELFSectionName (Left name)} = name
sectionName _ = "Unresolved name"

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

parseAddress :: ParseElf Address
parseAddress = do
    state <- F.getState
    case elfSize state of
        F.S32 -> do
            w <- F.parseWord
            return $ Address (Left w)
        F.S64 -> do
            w <- F.parseGWord
            return $ Address (Right w)
{-|
    This funcition parse the ELF header extracting all the usefull information
 -}
parseHeader :: ParseElf ELFHeader
parseHeader = do
        m <- parseHeaderMagic
        f <- parseHeaderClass
        endian <- parseHeaderEndianness
        v <- parseHeaderVersion
        abi <- parseHeaderABI
        F.skip 8
        t <- parseHeaderType
        arch <- parseHeaderMachine
        F.skip 4
        e <- parseAddress
        ph <- parseAddress
        sh <- parseAddress
        flgs <- F.parseWord
        hs <- F.parseHalf
        phes <- F.parseHalf
        phn <- F.parseHalf
        shes <- F.parseHalf
        shn <- F.parseHalf
        shsi <- F.parseHalf
        return ELFHeader {magic=m, format=f, fileEndianness=endian, version=v, osabi=abi, objectType=t, machine=arch, entry=e, phoff=ph, shoff=sh, flags=flgs, hsize=hs, phentsize=phes, phnum=phn, shentsize=shes, shnum=shn, shstrndx=shsi}

parseOffset :: ParseElf Offset
parseOffset = do
    state <- F.getState
    case elfSize state of 
        F.S32 -> do 
            w <- F.parseWord
            return $ Offset (Left w)
        F.S64 -> do
            w <- F.parseGWord
            return $ Offset (Right w) 

parseMachineInt :: ParseElf MachineInt
parseMachineInt = do
    state <- F.getState
    case elfSize state of 
        F.S32 -> do 
            w <- F.parseWord
            return $ MachineInt (Left w)
        F.S64 -> do
            w <- F.parseGWord
            return $ MachineInt (Right w) 

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
    pht <- parseProgramHeaderType
    pho <- parseOffset
    phv <- parseAddress
    php <- parseAddress
    phfs <- parseMachineInt
    phm <- parseMachineInt
    phf <- parseMachineInt
    pha <- parseMachineInt
    return ELFProgramHeader {phtype=pht, phoffset=pho, phvaddr=phv, phpaddr=php, phfilesz=phfs, phmemsz=phm, phflags=phf, phalign=pha}

parseSectionHeader :: ParseElf ELFSectionHeader
parseSectionHeader = do
    shn <- F.parseWord
    sht <- parseSectionHeaderType
    shflgs <- parseMachineInt
    sha <- parseAddress
    sho <-parseOffset
    shs <- parseMachineInt
    shl <- F.parseWord
    shi <- F.parseWord
    shaa <- parseMachineInt
    shes <- parseMachineInt
    return ELFSectionHeader {shname=ELFSectionName (Right shn), shtype=sht, shflags=shflgs, shaddr=sha, shoffset=sho, shsize=shs, shlink=shl, shinfo=shi, shaddralign=shaa, shentrysize=shes}

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

addressToInt :: Address -> Int
addressToInt (Address (Left i)) = fromIntegral i
addressToInt (Address (Right i)) = fromIntegral i

offsetToInt :: Offset -> Int
offsetToInt (Offset (Left i)) = fromIntegral i
offsetToInt (Offset (Right i)) = fromIntegral i

getSectionName :: ELFSectionHeader -> ParseElf ELFSectionHeader
getSectionName h@ELFSectionHeader {shname=ELFSectionName (Right d)} = do
    F.pushForwardTo $ fromIntegral d
    string <- parseString
    F.popFrom 
    return h {shname=ELFSectionName (Left string)} 

getAllSectionName :: [ELFSectionHeader] -> ParseElf [ELFSectionHeader]
getAllSectionName [] = return []
getAllSectionName (x:xs) = do
    sname <- getSectionName x
    (sname:) <$> (getAllSectionName xs)

discoverSectionNames :: ELFInfo -> ParseElf ELFInfo
discoverSectionNames info@ELFInfo {elfHeader=h, elfSectionHeaders=s} = do
    F.moveTo $ offsetToInt (shoffset (s !! (fromIntegral (shstrndx h))))
    sWithName <- getAllSectionName s
    return info {elfSectionHeaders=sWithName}

parseStringSection :: ELFSectionHeader -> ParseElf String
parseStringSection ELFSectionHeader {shoffset=off} = do
    F.moveTo $ offsetToInt off
    parseString
    
parseFile :: ParseElf ELFInfo
parseFile = do
    hdr <- parseHeader
    F.moveTo $ addressToInt (phoff hdr)
    phs <- parseProgramHeaders $ fromIntegral (phnum hdr)
    F.moveTo $ addressToInt (shoff hdr)
    shs <- parseSectionHeaders $ fromIntegral (shnum hdr)
    state <- F.getState
    discoverSectionNames $ ELFInfo {elfHeader=hdr, elfProgramHeaders=phs, elfSectionHeaders=shs, elfFileSize=(B.length (F.string state))}

isCommentSection :: ELFSectionHeader -> Bool
isCommentSection ELFSectionHeader {shname=ELFSectionName (Left s)}
    | s == ".comment"   = True
    | otherwise         = False
isCommentSection ELFSectionHeader {shname=ELFSectionName (Right s)} = False


dumpComment :: ELFInfo -> ParseElf String
dumpComment ELFInfo {elfSectionHeaders=shs} = do
    case find isCommentSection  shs of
        Just s -> parseStringSection s
        Nothing -> F.bail "Comment not found"

parse :: ParseElf a -> B.ByteString -> Either String a 
parse parser string = F.parse ParseState {elfOffset=0, elfSize=F.S32, elfEndianness=F.LittleEndian, elfString=string, elfOffsetState=[] } parser string



