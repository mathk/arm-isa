-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import Data.Binary.Strict.Util
import Text.Printf
import Data.Word

data ELF_Header_Magic = ELF_Header_Magic Word8 String

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

parse :: B.ByteString -> IO (Either String Word8, B.ByteString)
parse input = G.runGet parseElfHeaderMagic input >>? 
	(\c (ELF_Header_Magic w t) -> do 
		putStrLn $printf "0x%02X" w
		return $ G.runGet parseElfClass c)

main = do 
	input <- B.readFile "linker"
	(a, _) <- parse input
	case a of 
		Right _ -> putStrLn "Parse succeed"
		Left d -> putStrLn d
{-	let r = G.runGet parseElfHeaderMagic input 
	case fst r of
		Right (ELF_Header_Magic magic0 magic1) -> do 
			putStrLn $ printf "0x%02X" magic0
			putStrLn magic1
			case fst $ G.runGet parseElfClass (snd r) of
				Right a -> putStrLn $ printf "%d" a
				Left a -> putStrLn a
		Left a -> putStrLn a
	
-}
