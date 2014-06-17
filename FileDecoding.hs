-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import Control.Applicative
import Data.Binary.Strict.Util
import Data.Bits
import Data.Char (chr, isDigit, isSpace, isAlphaNum)
import Data.Int (Int64)
import Text.Printf
import Data.Word

{- Data type -}
data ParseState = ParseState {
        string :: B.ByteString,
        offset :: Int64,
        stateEndianness :: ELFHeaderData,
        size :: ELFHeaderClass
    } deriving (Show)

data ELFHeaderMagic = ELFHeaderMagic Word8 String

data ELFHeaderClass = ELF32 | ELF64

data ELFHeaderData = ELFBigEndian | ELFLittleEndian

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
        format :: ELFHeaderClass,
        endianness :: ELFHeaderData,
        version :: ELFHeaderVersion,
        osabi :: ELFHeaderABI,
        objectType :: ELFHeaderType,
        machine ::  ELFHeaderMachine,
        entry :: Address,
        phoff :: Address,
        shoff :: Address,
        flags :: Word32
    }

newtype Parse a = Parse {
        runParse :: ParseState -> Either String (a, ParseState)
    }

newtype Address = Address (Either Word32 Word64)

instance Show Address where
    show (Address (Left w)) = printf "0x%08X" w
    show (Address (Right w)) = printf "0x%016X" w

instance Show ELFHeaderMagic where
    show (ELFHeaderMagic w s) = printf "0x%02X %s" w s

instance Show ELFHeaderClass where
    show ELF32 = "32bit app"
    show ELF64 = "64bit app"

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
    show ELFHeader { magic=m, format=c, endianness=e, version=v, osabi=abi, objectType=t, machine=arch, entry=ent, phoff=ph, shoff=sh, flags=f} =
        printf "Magic: %s\nClass: %s\nEndianness: %s\nVersion: %s\nOSABI: %s\nType: %s\nMachine: %s\nEntry point: %s\nPhoff: %s\nShoff: %s\nFlags: 0x%08X"
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

instance Show ELFHeaderData where
    show ELFLittleEndian = "Little Endian"
    show ELFBigEndian    = "Big Endian"

instance Show ELFHeaderABI where
    show (ELFHeaderABI abi) = printf "ABI(0x%02X)" abi

{- Parser composition -}
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) ->
                    runParse (secondParser firstResult) newState

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

{- Parse functor -}
instance Functor Parse where
    fmap f parser = parser ==> \result ->
        identity (f result)

{- Parser Utils -}
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

{- Parse primitive -}
w8tow16 :: Word8 -> Word8 -> Word16
w8tow16 mosteByte lessByte = fromIntegral lessByte + (fromIntegral mosteByte `shiftL` 8)

w16tow32 :: Word16 -> Word16 -> Word32
w16tow32 mosteBytes lessBytes =
        fromIntegral lessBytes + (fromIntegral mosteBytes `shiftL` 16)

w8tow32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w8tow32 mosteByte1 mosteByte2 lessByte1 lessByte2 =
        fromIntegral $ w8tow16 lessByte1 lessByte2 + (fromIntegral $ w8tow16 mosteByte1 mosteByte2 `shiftL` 16)

w32tow64 :: Word32 -> Word32 -> Word64
w32tow64 mosteWord lessWord = fromIntegral lessWord + (fromIntegral mosteWord `shiftL` 32)

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
        case B.uncons (string initState) of
            Nothing -> bail "no more input"
            Just (byte, remainder) ->
                  Parse (\_ -> Right(byte, newState))
             where newState = initState { string = remainder,
                                             offset = newOffset }
                   newOffset = offset initState + 1

parseHalf :: Parse Word16
parseHalf = getState ==> \state ->
    case stateEndianness state of
        ELFLittleEndian ->
            parseByte ==> \lessByte ->
            parseByte ==> \mostByte ->
                identity $ w8tow16 mostByte lessByte
        ELFBigEndian ->
            parseByte ==> \mostByte ->
            parseByte ==> \lessByte ->
                identity $ w8tow16 mostByte lessByte

parseWord :: Parse Word32
parseWord = getState ==> \state ->
    case stateEndianness state of
        ELFLittleEndian ->
            parseHalf ==> \lessBytes ->
            parseHalf ==> \mosteBytes ->
                identity $ w16tow32 mosteBytes lessBytes
        ELFBigEndian ->
            parseHalf ==> \mosteBytes ->
            parseHalf ==> \lessBytes ->
                identity $ w16tow32 mosteBytes lessBytes

parseGWord :: Parse Word64
parseGWord = getState ==> \state ->
    case stateEndianness state of 
        ELFLittleEndian ->
            parseWord ==> \lessWord ->
            parseWord ==> \mosteWord ->
                identity $ w32tow64 mosteWord lessWord
        ELFBigEndian ->
            parseWord ==> \mosteWord ->
            parseWord ==> \lessWord ->
                identity $ w32tow64 mosteWord lessWord

skip :: Int -> Parse ()
skip 0 = identity ()
skip n
    | n > 0     = parseByte ==>&
                    skip (n - 1)
    | otherwise = bail "Can not skip negative amount of byte"

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . B.uncons . string) <$> getState

{-  :type fmap $ fmap w2c -}
peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []

parseIdentifier :: Parse String
parseIdentifier = fmap w2c <$> parseWhile (isAlphaNum . w2c)

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

{- ELf specific routine -}
parseELFHeaderClass :: Parse ELFHeaderClass
parseELFHeaderClass = parseByte ==> \b ->
        case b of
            1 -> getState ==> \state -> 
                 putState state {size = ELF32} ==>& identity ELF32
            2 -> getState ==> \state ->
                 putState state {size = ELF64} ==>& identity ELF64
            _ -> bail $ printf "Unknown class (0x02X)" b

parseELFHeaderMagic :: Parse ELFHeaderMagic
parseELFHeaderMagic = parseByte ==> \magicByte ->
        assert (magicByte == 0x7F) "First magic byte is wrong" ==>&
        parseIdentifier ==> \ident ->
            assert (ident == "ELF") "Magic string is not ELF" ==>&
            identity (ELFHeaderMagic magicByte ident)

parseELFHeaderEndianness :: Parse ELFHeaderData
parseELFHeaderEndianness = parseByte ==> \b ->
            case b of
                1 -> getState ==> \state ->
                     putState state {stateEndianness = ELFLittleEndian } ==>& identity ELFLittleEndian
                0 -> getState ==> \state ->
                     putState state {stateEndianness = ELFBigEndian} ==>& identity ELFBigEndian
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
        ELF32 -> parseWord ==> \w -> identity $ Address (Left w)
        ELF64 -> parseGWord ==> \w -> identity $ Address (Right w)

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

{- Parse engine that chain all the parser -}
parse :: Parse a -> B.ByteString -> Either String a
parse parser input =
    case runParse parser (ParseState input 0 ELFLittleEndian ELF32) of
        Left err            -> Left err
        Right (result, _)   -> Right result


main = do
    input <- B.readFile "linker"
    case parse parseELFHeader  input of
        Right value -> putStrLn $ show value
        Left d -> putStrLn d

