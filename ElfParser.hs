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
      parseELFHeader,
      parseELFFile,
      parseElf
    ) where

import qualified Data.ByteString as B
import Control.Applicative
import FileDecoding
import Text.Printf
import Data.Word

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
        format :: AddressSize,
        fileEndianness :: Endianness,
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
        elfSectionHeaders :: [ELFSectionHeader]
    } deriving (Show)

data ParseElfState = ParseElfState {
        elfOffset :: Int,
        elfString :: B.ByteString,
        elfEndianness :: Endianness,
        elfSize :: AddressSize,
        elfOffsetState :: [Int]
    }

newtype Address = Address (Either Word32 Word64)
newtype Offset = Offset (Either Word32 Word64)
newtype MachineInt = MachineInt (Either Word32 Word64)

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
        printf "Magic: %s\nClass: %s\nEndianness: %s\nVersion: %s\nOSABI: %s\nType: %s\nMachine: %s\nEntry point: %s\nPhoff: %s\nShoff: %s\nFlags: 0x%08X\nHeader Size: %d\nProgram Header Size: %d\nProgram Header Entry Number: %d\nSection Header Size: %d\nSection Header Entry Number: %d\nIndex Section Name: %d"
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
        printf "Program Header Type: %s\nProgram Header Offset: %s\nVirtual Address: %s\nPhysical Address: %s\nSegment File Size: %s\nSegment Memory Size: %s\nFlags: %s\nSegment Alignment: %s"
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
        printf "Section Name: %s\nSection Type: %s\nSection Flags: %s\nSection Address: %s\nSection Offset: %s\nSection Size: %s\nSection Link: %s\nSection Info: %s\nSection Address Align: %s\nSection Entry Size: %s"
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


instance ParseStateAccess ParseElfState where
    offset = elfOffset
    string = elfString
    endianness = elfEndianness
    putOffset a off = a { elfOffset = off }
    pushOffset a@ParseElfState {elfOffsetState=x} off = a { elfOffset=off, elfOffsetState=(elfOffset a):x}
    popOffset a@ParseElfState {elfOffsetState=x:xs} = a {elfOffset=x, elfOffsetState=xs} 
    

{- ELf specific routine -}
parseELFHeaderClass :: Parse ParseElfState AddressSize
parseELFHeaderClass = parseByte ==> \b ->
        case b of
            1 -> getState ==> \state -> 
                 putState state {elfSize = S32} ==>& identity S32
            2 -> getState ==> \state ->
                 putState state {elfSize = S64} ==>& identity S64
            _ -> bail $ printf "Unknown class (0x02X)" b

parseELFHeaderMagic :: Parse ParseElfState ELFHeaderMagic
parseELFHeaderMagic = parseByte ==> \magicByte ->
        assert (magicByte == 0x7F) "First magic byte is wrong" ==>&
        parseIdentifier ==> \ident ->
            assert (ident == "ELF") (printf "Magic string is not ELF %s" ident) ==>&
            identity (ELFHeaderMagic magicByte ident)

parseELFHeaderEndianness :: Parse ParseElfState Endianness
parseELFHeaderEndianness = parseByte ==> \b ->
            case b of
                1 -> getState ==> \state ->
                     putState state {elfEndianness = LittleEndian } ==>& identity LittleEndian
                0 -> getState ==> \state ->
                     putState state {elfEndianness = BigEndian} ==>& identity BigEndian
                _ -> bail $ printf "Bad endianness (0x%02X)" b

parseELFHeaderType :: Parse ParseElfState ELFHeaderType
parseELFHeaderType = parseHalf ==> \b ->
        case b of
            1 -> identity ELFRelocatable
            2 -> identity ELFExecutable
            3 -> identity ELFShared
            4 -> identity ELFCore
            _ -> bail $ printf "Bad elf type (0x%02X)" b

parseELFHeaderMachine :: Parse ParseElfState ELFHeaderMachine
parseELFHeaderMachine = parseHalf ==> \b ->
        case b of
            0x02 -> identity ELFSPARC
            0x03 -> identity ELFx86
            0x08 -> identity ELFMIPS
            0x14 -> identity ELFPowerPC
            0x28 -> identity ELFARM
            0x2A -> identity ELFSuperH
            0x32 -> identity ELFIA64
            0x3E -> identity ELFx86_64
            0xB7 -> identity ELFAArch64
            _ -> bail $ printf "Unknown machine (0x%02X)" b

parseELFHeaderVersion :: Parse ParseElfState ELFHeaderVersion
parseELFHeaderVersion = parseByte ==> \b ->
        case b of
            1 -> identity ELFDefaultVersion
            _ -> identity ELFOtherVersion

parseELFHeaderABI :: Parse ParseElfState ELFHeaderABI
parseELFHeaderABI = parseByte ==> \b ->
        identity (ELFHeaderABI b)

parseELFAddress :: Parse ParseElfState Address
parseELFAddress = getState ==> \state ->
    case elfSize state of
        S32 -> parseWord ==> \w -> identity $ Address (Left w)
        S64 -> parseGWord ==> \w -> identity $ Address (Right w)
{-|
    This funcition parse the ELF header extracting all the usefull information
 -}
parseELFHeader :: Parse ParseElfState ELFHeader
parseELFHeader = do
        m <- parseELFHeaderMagic
        f <- parseELFHeaderClass
        endian <- parseELFHeaderEndianness
        v <- parseELFHeaderVersion
        abi <- parseELFHeaderABI
        skip 8
        t <- parseELFHeaderType
        arch <- parseELFHeaderMachine
        skip 4
        e <- parseELFAddress
        ph <- parseELFAddress
        sh <- parseELFAddress
        flgs <- parseWord
        hs <- parseHalf
        phes <- parseHalf
        phn <- parseHalf
        shes <- parseHalf
        shn <- parseHalf
        shsi <- parseHalf
        return ELFHeader {magic=m, format=f, fileEndianness=endian, version=v, osabi=abi, objectType=t, machine=arch, entry=e, phoff=ph, shoff=sh, flags=flgs, hsize=hs, phentsize=phes, phnum=phn, shentsize=shes, shnum=shn, shstrndx=shsi}

parseELFOffset :: Parse ParseElfState Offset
parseELFOffset = do
    state <- getState
    case elfSize state of 
        S32 -> do 
            w <- parseWord
            return $ Offset (Left w)
        S64 -> do
            w <- parseGWord
            return $ Offset (Right w) 

parseELFMachineInt :: Parse ParseElfState MachineInt
parseELFMachineInt = do
    state <- getState
    case elfSize state of 
        S32 -> do 
            w <- parseWord
            return $ MachineInt (Left w)
        S64 -> do
            w <- parseGWord
            return $ MachineInt (Right w) 

parseELFProgramHeaderType :: Parse ParseElfState ELFProgramHeaderType
parseELFProgramHeaderType = do
    w <- parseWord
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
        _ -> bail $ printf "Unrecognized program header type 0x%08X" w

parseELFSectionHeaderType :: Parse ParseElfState ELFSectionHeaderType
parseELFSectionHeaderType = do
    w <- parseWord
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
        _ -> bail $ printf "Unrecognized section header type 0x%08X" w

parseELFString :: Parse ParseElfState String
parseELFString = fmap w2c <$> parseWhile (\w -> not $ w == 0)

parseELFProgramHeader :: Parse ParseElfState ELFProgramHeader
parseELFProgramHeader = do
    pht <- parseELFProgramHeaderType
    pho <- parseELFOffset
    phv <- parseELFAddress
    php <- parseELFAddress
    phfs <- parseELFMachineInt
    phm <- parseELFMachineInt
    phf <- parseELFMachineInt
    pha <- parseELFMachineInt
    return ELFProgramHeader {phtype=pht, phoffset=pho, phvaddr=phv, phpaddr=php, phfilesz=phfs, phmemsz=phm, phflags=phf, phalign=pha}

parseELFSectionHeader :: Parse ParseElfState ELFSectionHeader
parseELFSectionHeader = do
    shn <- parseWord
    sht <- parseELFSectionHeaderType
    shflgs <- parseELFMachineInt
    sha <- parseELFAddress
    sho <-parseELFOffset
    shs <- parseELFMachineInt
    shl <- parseWord
    shi <- parseWord
    shaa <- parseELFMachineInt
    shes <- parseELFMachineInt
    return ELFSectionHeader {shname=ELFSectionName (Right shn), shtype=sht, shflags=shflgs, shaddr=sha, shoffset=sho, shsize=shs, shlink=shl, shinfo=shi, shaddralign=shaa, shentrysize=shes}

parseELFArray :: Parse ParseElfState a -> Int -> Parse ParseElfState [a]
parseELFArray parser 0 = return []
parseELFArray parser n
    | n > 0 = do 
        h <- parser
        (h:) <$> (parseELFArray parser (n - 1))
    | otherwise = bail "Can not parse negative number of array element" 

parseELFProgramHeaders :: Int -> Parse ParseElfState [ELFProgramHeader]
parseELFProgramHeaders = parseELFArray parseELFProgramHeader

parseELFSectionHeaders :: Int -> Parse ParseElfState [ELFSectionHeader]
parseELFSectionHeaders = parseELFArray parseELFSectionHeader

addressToInt :: Address -> Int
addressToInt (Address (Left i)) = fromIntegral i
addressToInt (Address (Right i)) = fromIntegral i

offsetToInt :: Offset -> Int
offsetToInt (Offset (Left i)) = fromIntegral i
offsetToInt (Offset (Right i)) = fromIntegral i

getELFSectionName :: ELFSectionHeader -> Parse ParseElfState ELFSectionHeader
getELFSectionName h@ELFSectionHeader {shname=ELFSectionName (Right d)} = do
    pushForwardTo $ fromIntegral d
    string <- parseELFString
    popFrom 
    return h {shname=ELFSectionName (Left string)} 

getAllELFSectionName :: [ELFSectionHeader] -> Parse ParseElfState [ELFSectionHeader]
getAllELFSectionName [] = identity []
getAllELFSectionName (x:xs) = do
    sname <- getELFSectionName x
    (sname:) <$> (getAllELFSectionName xs)

discoverELFSectionNames :: ELFInfo -> Parse ParseElfState ELFInfo
discoverELFSectionNames info@ELFInfo {elfHeader=h, elfSectionHeaders=s} = do
    moveTo $ offsetToInt (shoffset (s !! (fromIntegral (shstrndx h))))
    sWithName <- getAllELFSectionName s
    return info {elfSectionHeaders=sWithName}
    
parseELFFile :: Parse ParseElfState ELFInfo
parseELFFile = do
    hdr <- parseELFHeader
    moveTo $ addressToInt (phoff hdr)
    phs <- parseELFProgramHeaders $ fromIntegral (phnum hdr)
    moveTo $ addressToInt (shoff hdr)
    shs <- parseELFSectionHeaders $ fromIntegral (shnum hdr)
    discoverELFSectionNames $ ELFInfo {elfHeader=hdr, elfProgramHeaders=phs, elfSectionHeaders=shs}

parseElf :: Parse ParseElfState a -> B.ByteString -> Either String a 
parseElf parser string = parse ParseElfState {elfOffset=0, elfSize=S32, elfEndianness=LittleEndian, elfString=string, elfOffsetState=[] } parser string

