{-|
    Module          : ElfParser
    Description     : ELF Parser
    Copyright       : (c) Mathieu Suen, 2014
    License         : MIT
    Maintainer      : mathk.sue@gmail.com
 -}
module ElfParser
    (
      parseELFHeader
    ) where

import FileDecoding
import Text.Printf
import Data.Word

{- ELF Data type -}
data ELFHeaderMagic = ELFHeaderMagic Word8 String

data ELFHeaderVersion = ELFDefaultVersion | ELFOtherVersion

data ELFHeaderABI = ELFHeaderABI Word8

data ELFHeaderType = ELFRelocatable | ELFExecutable | ELFShared | ELFCore

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
        endianness :: Endianness,
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

newtype Address = Address (Either Word32 Word64)

{- Instance declaration -}
instance Show Address where
    show (Address (Left w)) = printf "0x%08X" w
    show (Address (Right w)) = printf "0x%016X" w

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

instance Show ELFHeader where
    show ELFHeader { magic=m, format=c, endianness=e, version=v, osabi=abi, objectType=t, machine=arch, entry=ent, phoff=ph, shoff=sh, flags=f, hsize=hs, phentsize=phes, phnum=phn, shentsize=shes, shnum=shn, shstrndx=shsi} =
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

instance Show ELFHeaderABI where
    show (ELFHeaderABI abi) = printf "ABI(0x%02X)" abi

{- ELf specific routine -}
parseELFHeaderClass :: Parse AddressSize
parseELFHeaderClass = parseByte ==> \b ->
        case b of
            1 -> getState ==> \state -> 
                 putState state {size = S32} ==>& identity S32
            2 -> getState ==> \state ->
                 putState state {size = S64} ==>& identity S64
            _ -> bail $ printf "Unknown class (0x02X)" b

parseELFHeaderMagic :: Parse ELFHeaderMagic
parseELFHeaderMagic = parseByte ==> \magicByte ->
        assert (magicByte == 0x7F) "First magic byte is wrong" ==>&
        parseIdentifier ==> \ident ->
            assert (ident == "ELF") "Magic string is not ELF" ==>&
            identity (ELFHeaderMagic magicByte ident)

parseELFHeaderEndianness :: Parse Endianness
parseELFHeaderEndianness = parseByte ==> \b ->
            case b of
                1 -> getState ==> \state ->
                     putState state {stateEndianness = LittleEndian } ==>& identity LittleEndian
                0 -> getState ==> \state ->
                     putState state {stateEndianness = BigEndian} ==>& identity BigEndian
                _ -> bail $ printf "Bad endianness (0x%02X)" b

parseELFHeaderType :: Parse ELFHeaderType
parseELFHeaderType = parseHalf ==> \b ->
        case b of
            1 -> identity ELFRelocatable
            2 -> identity ELFExecutable
            3 -> identity ELFShared
            4 -> identity ELFCore
            _ -> bail $ printf "Bad elf type (0x%02X)" b

parseELFHeaderMachine :: Parse ELFHeaderMachine
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

parseELFHeaderVersion :: Parse ELFHeaderVersion
parseELFHeaderVersion = parseByte ==> \b ->
        case b of
            1 -> identity ELFDefaultVersion
            _ -> identity ELFOtherVersion

parseELFHeaderABI :: Parse ELFHeaderABI
parseELFHeaderABI = parseByte ==> \b ->
        identity (ELFHeaderABI b)

parseELFAddress :: Parse Address
parseELFAddress = getState ==> \state ->
    case size state of
        S32 -> parseWord ==> \w -> identity $ Address (Left w)
        S64 -> parseGWord ==> \w -> identity $ Address (Right w)
{-|
    This funcition parse the ELF header extracting all the usefull information
 -}
parseELFHeader :: Parse ELFHeader
parseELFHeader = parseELFHeaderMagic ==> \m ->
        parseELFHeaderClass ==> \f ->
        parseELFHeaderEndianness ==> \endian ->
        parseELFHeaderVersion ==> \v ->
        parseELFHeaderABI ==> \abi ->
        skip 8 ==>&
        parseELFHeaderType ==> \t ->
        parseELFHeaderMachine ==> \arch ->
        skip 4 ==>&
        parseELFAddress ==> \e ->
        parseELFAddress ==> \ph ->
        parseELFAddress ==> \sh ->
        parseWord ==> \flgs ->
        parseHalf ==> \hs ->
        parseHalf ==> \phes ->
        parseHalf ==> \phn ->
        parseHalf ==> \shes ->
        parseHalf ==> \shn ->
        parseHalf ==> \shsi ->
           identity ELFHeader {magic=m, format=f, endianness=endian, version=v, osabi=abi, objectType=t, machine=arch, entry=e, phoff=ph, shoff=sh, flags=flgs, hsize=hs, phentsize=phes, phnum=phn, shentsize=shes, shnum=shn, shstrndx=shsi}

