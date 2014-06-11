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

main :: IO ()
main = do 
	input <- B.readFile "linker"
	let r = G.runGet parseElfHeaderMagic input 
	case fst r of
		Right (ELF_Header_Magic magic0 magic1) -> do 
			putStrLn $ printf "0x%02X" magic0
			putStrLn magic1
			case fst $ G.runGet parseElfClass (snd r) of
				Right a -> putStrLn $ printf "%d" a
				Left a -> putStrLn a
		Left a -> putStrLn a
	

