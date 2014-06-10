-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data
import qualified Data.ByteString as B
import Text.Printf
import Data.Binary.Strict.Get
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word

dh :: Get (Word32, Word32, Word32)
dh = do
	alen <- getWord32be
	plen <- getWord32be
	chksum <- getWord32be
	return (alen, plen, chksum)

parseElfHeader :: BG.BitGet Word8
parseElfHeader = BG.getAsWord8 8

main :: IO ()
main = do 
	input <- B.readFile "linker"
	case BG.runBitGet input parseElfHeader of
		Right a -> putStrLn $ printf "0x%02X" a
		Left a -> putStrLn ""
	

