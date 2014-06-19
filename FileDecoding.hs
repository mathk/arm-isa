{-|
    Module          : FileDecoding
    Description     : Utilities to decode binary file
    Copyright       : (c) Mathieu Suen, 2014
    License         : MIT
    Maintainer      : mathk.sue@gmail.com
 -}
module FileDecoding
    (
      Parse(..),
      Endianness(..),
      AddressSize(..),
      ParseStateAccess(..),
      (==>),
      (==>&),
      parse,
      identity,
      parseByte,
      parseHalf,
      parseWord,
      parseGWord,
      parseIdentifier,
      bail,
      skip,
      moveTo,
      getState,
      putState,
      assert
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

{-| 
    Class used to carry the state along when parsing the file.
 -}
class ParseStateAccess a where
    offset :: a -> Int
    string :: a -> B.ByteString
    endianness :: a -> Endianness
    putOffset :: a -> Int -> a

data Endianness = BigEndian | LittleEndian

data AddressSize = S32 | S64

newtype Parse b a = Parse {
        runParse :: b -> Either String (a, b)
    }

{- Instance declaration -}
instance Show Endianness where
    show LittleEndian = "Little Endian"
    show BigEndian    = "Big Endian"

instance Show AddressSize where
    show S32 = "32bit app"
    show S64 = "64bit app"


{- Parser composition -}
(==>) :: Parse s a -> (a -> Parse s b) -> Parse s b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) ->
                    runParse (secondParser firstResult) newState

(==>&) :: Parse s a -> Parse s b -> Parse s b
p ==>& f = p ==> \_ -> f

{- Parse functor -}
instance Functor (Parse a) where
    fmap f parser = parser ==> \result ->
        identity (f result)

instance (ParseStateAccess a) => Monad (Parse a) where
    return = identity
    (>>=) = (==>)
    fail = bail

{- Parser Utils -}
getState :: Parse a a
getState = Parse (\s -> Right (s, s))

putState :: a -> Parse a ()
putState s = Parse (\_ -> Right((), s))

isInRange :: Int -> B.ByteString -> Bool
isInRange n string = n >= 0 && (B.length string) < n 

{-|
    Stop the parser and report an error.
-}
bail :: (ParseStateAccess s) => String -> Parse s a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

identity :: a -> Parse s a
identity a = Parse (\s -> Right (a, s))

{- Parse primitive -}
w8tow16 :: Word8 -> Word8 -> Word16
w8tow16 mosteByte lessByte = 
    fromIntegral lessByte + (fromIntegral mosteByte `shiftL` 8)

w16tow32 :: Word16 -> Word16 -> Word32
w16tow32 mosteBytes lessBytes =
    fromIntegral lessBytes + (fromIntegral mosteBytes `shiftL` 16)

w32tow64 :: Word32 -> Word32 -> Word64
w32tow64 mosteWord lessWord = 
    fromIntegral lessWord + (fromIntegral mosteWord `shiftL` 32)

parseByte :: (ParseStateAccess s) => Parse s Word8
parseByte = do
    initState <- getState
    assert ((offset initState) < B.length (string initState)) "no more input"
    putState $ putOffset initState (offset initState + 1)
    return $ B.index (string initState) (offset initState)

parseHalf :: (ParseStateAccess s) => Parse s Word16
parseHalf = getState ==> \state ->
    case endianness state of
        LittleEndian ->
            parseByte ==> \lessByte ->
            parseByte ==> \mostByte ->
                identity $ w8tow16 mostByte lessByte
        BigEndian ->
            parseByte ==> \mostByte ->
            parseByte ==> \lessByte ->
                identity $ w8tow16 mostByte lessByte

parseWord :: (ParseStateAccess s) => Parse s Word32
parseWord = getState ==> \state ->
    case endianness state of
        LittleEndian ->
            parseHalf ==> \lessBytes ->
            parseHalf ==> \mosteBytes ->
                identity $ w16tow32 mosteBytes lessBytes
        BigEndian ->
            parseHalf ==> \mosteBytes ->
            parseHalf ==> \lessBytes ->
                identity $ w16tow32 mosteBytes lessBytes

parseGWord :: (ParseStateAccess s) => Parse s Word64
parseGWord = getState ==> \state ->
    case endianness state of 
        LittleEndian ->
            parseWord ==> \lessWord ->
            parseWord ==> \mosteWord ->
                identity $ w32tow64 mosteWord lessWord
        BigEndian ->
            parseWord ==> \mosteWord ->
            parseWord ==> \lessWord ->
                identity $ w32tow64 mosteWord lessWord

skip :: (ParseStateAccess s) => Int -> Parse s ()
skip 0 = identity ()
skip n
    | n > 0     = parseByte ==>&
                    skip (n - 1)
    | otherwise = bail "Can not skip negative amount of byte"

{-|
    Move to an arbitrary location in the file. 
 -}
moveTo :: (ParseStateAccess s) => Int -> Parse s ()
moveTo n = do
    state <- getState
    assert (not $ isInRange n (string state))  (printf "Displacement is out of range %d. Expected [0,%d]" n (B.length $ string state))
    putState $ putOffset state n

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: (ParseStateAccess s) => Parse s Char
parseChar = w2c <$> parseByte

peekByte :: (ParseStateAccess s) => Parse s (Maybe Word8)
peekByte = do
    state <- getState
    if (offset state) >= B.length (string state)
    then return Nothing
    else return (Just (B.index (string state) (offset state)))

peekChar :: (ParseStateAccess s) => Parse s (Maybe Char)
peekChar = fmap w2c <$> peekByte

{-|
    Parse until a condition is met and report the result as list of byte.
    The result can be an empty list.
-}
parseWhile :: (ParseStateAccess s) => (Word8 -> Bool) -> Parse s [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []
{-|
    Parse a continuous byte of alpha numeric character and report it as a String.
 -}
parseIdentifier :: (ParseStateAccess s) => Parse s String
parseIdentifier = fmap w2c <$> parseWhile (isAlphaNum . w2c)

{-|
    Assert a condition and stop parsing if the condition is not met.
    It report the text error to the client.
 -}
assert :: (ParseStateAccess s) => Bool -> String -> Parse s ()
assert True  _   = identity ()
assert False err = bail err

{-|
  Parse engine that chain all the parser 
-}
parse :: b -> Parse b a -> B.ByteString -> Either String a
parse initState parser input =
    case runParse parser initState of
        Left err            -> Left err
        Right (result, _)   -> Right result


