-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import Data.Binary.Strict.Util
import Data.Int (Int64)
import Text.Printf
import Data.Word

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

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right((), s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) ->
                    runParse (secondParser firstResult) newState

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset" ++ show (offset s) ++ ": " ++ err

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

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

parse :: Parse a -> B.ByteString -> Either String a
parse parser input =
    case runParse parser (ParseState input 0) of
        Left err    ->Left err
        Right (result, _) -> Right result

parseElfHeaderMagic :: G.Get ELF_Header_Magic
parseElfHeaderMagic = do 
    magicByte <- G.getWord8 
    magicString <- G.getByteString 3
    return $ ELF_Header_Magic magicByte (C.unpack magicString)

parseElfClass :: G.Get Word8
parseElfClass = G.getWord8

(>>?) :: (Either String b, B.ByteString) -> (B.ByteString -> b -> IO (Either String c, B.ByteString)) -> IO (Either String c, B.ByteString)
(Right r, c) >>? f  = f c r
(Left a, c) >>? f = do return (Left a, c) 

oldparse :: B.ByteString -> IO (Either String Word8, B.ByteString)
oldparse input = G.runGet parseElfHeaderMagic input >>? 
    (\c h -> do 
        putStrLn $ show h
        return $ G.runGet parseElfClass c)

main = do 
    input <- B.readFile "linker"
    (a, _) <- oldparse input
    case a of 
        Right _ -> putStrLn "Parse succeed"
        Left d -> putStrLn d

