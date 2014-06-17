module FileDecoding
    (
      Parse(..),
      Endianness(..),
      (==>),
      (==>&),
      parse
    ) where

-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Data.Bits
import Data.Char (chr, isDigit, isSpace, isAlphaNum)
import Data.Int (Int64)
import Text.Printf
import Data.Word

{- Data type -}
data ParseState = ParseState {
        string :: B.ByteString,
        offset :: Int64,
        stateEndianness :: Endianness,
        size :: ELFHeaderClass
    } deriving (Show)

data Endianness = BigEndian | LittleEndian

newtype Parse a = Parse {
        runParse :: ParseState -> Either String (a, ParseState)
    }

instance Show Endianness where
    show LittleEndian = "Little Endian"
    show BigEndian    = "Big Endian"

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
        LittleEndian ->
            parseByte ==> \lessByte ->
            parseByte ==> \mostByte ->
                identity $ w8tow16 mostByte lessByte
        BigEndian ->
            parseByte ==> \mostByte ->
            parseByte ==> \lessByte ->
                identity $ w8tow16 mostByte lessByte

parseWord :: Parse Word32
parseWord = getState ==> \state ->
    case stateEndianness state of
        LittleEndian ->
            parseHalf ==> \lessBytes ->
            parseHalf ==> \mosteBytes ->
                identity $ w16tow32 mosteBytes lessBytes
        BigEndian ->
            parseHalf ==> \mosteBytes ->
            parseHalf ==> \lessBytes ->
                identity $ w16tow32 mosteBytes lessBytes

parseGWord :: Parse Word64
parseGWord = getState ==> \state ->
    case stateEndianness state of 
        LittleEndian ->
            parseWord ==> \lessWord ->
            parseWord ==> \mosteWord ->
                identity $ w32tow64 mosteWord lessWord
        BigEndian ->
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

{- Parse engine that chain all the parser -}
parse :: Parse a -> B.ByteString -> Either String a
parse parser input =
    case runParse parser (ParseState input 0 LittleEndian ELF32) of
        Left err            -> Left err
        Right (result, _)   -> Right result


