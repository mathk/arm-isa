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
      w2c,
      parse,
      identity,
      parseByte,
      parseHalf,
      parseWord,
      parseGWord,
      parseIdentifier,
      parseWhile,
      parseRaw,
      bail,
      skip,
      moveTo,
      forwardTo,
      pushTo,
      pushForwardTo,
      popFrom,
      getState,
      putState,
      assert,
      canMoveTo,
    ) where

-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char (chr, isDigit, isSpace, isAlphaNum)
import Data.Int (Int64)
import Text.Printf
import Data.Word
import Data.Int (Int64)
{-| 
    Class used to carry the state along when parsing the file.
 -}
class ParseStateAccess a where
    offset :: a -> Int64
    string :: a -> B.ByteString
    endianness :: a -> Endianness
    putOffset :: a -> Int64 -> a
    pushOffset :: a -> Int64 -> a
    popOffset :: a -> a

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

instance (ParseStateAccess a) => Applicative (Parse a) where
    pure = identity
    (<*>) = ap

{- Parser Utils -}
getState :: Parse a a
getState = Parse (\s -> Right (s, s))

putState :: a -> Parse a ()
putState s = Parse (\_ -> Right((), s))

isInRange :: Int64 -> B.ByteString -> Bool
isInRange n string = n >= 0 && fromIntegral (B.length string) > n 

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
    assert ((offset initState) < fromIntegral (B.length $ string initState)) "no more input"
    putState $ putOffset initState (offset initState + 1)
    return $ B.index (string initState) (fromIntegral $ offset initState)

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

-- | Tell if we can move to a specific location in the byte stream
canMoveTo :: (ParseStateAccess s) => Int64 -> Parse s Bool
canMoveTo n = do
    state <- getState
    return $ isInRange n (string state)

moveTo :: (ParseStateAccess s) => Int64 -> Parse s ()
moveTo n = do
    state <- getState
    assert (isInRange n (string state))  (printf "Displacement is out of range %d. Expected [0,%d]" n (B.length $ string state))
    putState $ putOffset state n

forwardTo :: (ParseStateAccess s) => Int64 -> Parse s ()
forwardTo n = do
    state <- getState
    moveTo (n +(offset state))

pushTo :: (ParseStateAccess s) => Int64 -> Parse s ()
pushTo n = do
    state <- getState
    assert (isInRange n (string state))  (printf "Displacement is out of range %d. Expected [0,%d]" n (B.length $ string state))
    putState $ pushOffset state n

pushForwardTo :: (ParseStateAccess s) => Int64 -> Parse s ()
pushForwardTo n = do
    state <- getState
    pushTo (n+(offset state))

popFrom :: (ParseStateAccess s) => Parse s ()
popFrom = do
    state <- getState
    putState $ popOffset state

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: (ParseStateAccess s) => Parse s Char
parseChar = w2c <$> parseByte

peekByte :: (ParseStateAccess s) => Parse s (Maybe Word8)
peekByte = do
    state <- getState
    if (offset state) >= fromIntegral (B.length $ string state)
    then return Nothing
    else return (Just (B.index (string state) (fromIntegral $ offset state)))

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

parseCount :: (ParseStateAccess s) => Int64 -> Parse s [Word8]
parseCount 0 = identity []
parseCount n = do
    b <- parseByte
    (b:) <$> parseCount (n-1)

parseRaw :: (ParseStateAccess s) => Int64 -> Parse s B.ByteString
parseRaw n = B.pack <$> parseCount n


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


