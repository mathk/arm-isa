-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import Control.Applicative
import Data.Binary.Strict.Util
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Text.Printf
import Data.Word

{- Data type -}
data ParseState = ParseState {
        string :: B.ByteString,
        offset :: Int64
    } deriving (Show)

data ELF_Header_Magic = ELF_Header_Magic Word8 String

instance Show ELF_Header_Magic where
    show (ELF_Header_Magic w s) = printf "0x%02X %s" w s

newtype Parse a = Parse {
        runParse :: ParseState -> Either String (a, ParseState)
    }

{- Parser composition -}
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) ->
                    runParse (secondParser firstResult) newState

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
bail err = Parse $ \s -> Left $ "byte offset" ++ show (offset s) ++ ": " ++ err

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

{- Parse Byte -}
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

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . B.uncons . string) <$> getState

{-  :type fmap $ fmap w2c -}
peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

{- Parse engine that chain all the parser -}
parse :: Parse a -> B.ByteString -> Either String a
parse parser input =
    case runParse parser (ParseState input 0) of
        Left err    ->Left err
        Right (result, _) -> Right result


main = do
    input <- B.readFile "linker"
    case parse parseByte  input of
        Right value -> putStrLn $ printf "Parse succeed. Header 0x%02X" value
        Left d -> putStrLn d

