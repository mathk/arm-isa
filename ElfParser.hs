module ElfParser
    (
      parseELFHeader
    ) where

import FileDecoding

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
           identity ELFHeader {magic=m, format=f, endianness=endian, version=v, osabi=abi, objectType=t, machine=arch, entry=e, phoff=ph, shoff=sh, flags=flgs}

