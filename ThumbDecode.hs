module ThumbDecode (
) where

import ArmDecode
import Data.Binary
import qualified Data.Binary.Get as Bin
import Data.Binary.Bits
import Data.Bits 
import Data.ByteString
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State.Lazy as S
import Text.Printf
import Data.Tuple.All

data ThumbStream = ThumbStream ByteString Word16 Word16

type ThumbStreamState a = S.State ThumbStream a

nextThumbInstruction :: ThumbStreamState ()
nextThumbInstruction = do
    (ThumbStream b ph pl) <- S.get
    case Bin.pushChunk (Bin.runGetIncremental Bin.getWord16le) b of
        Bin.Done resultS _ word -> do
            S.put $  ThumbStream resultS word pl
            instMark <- firstInstructionBits 11 5
            if instMark == 29 || instMark == 30 || instMark == 31
            then case Bin.pushChunk (Bin.runGetIncremental Bin.getWord16le) resultS of
                Bin.Done resultL _ wordLo -> do
                    S.put $ ThumbStream resultL word wordLo 
            else return ()
    
-- | Get the value of a field in a instruction stream
-- Since thumb can be compose of 2 half word this function work on
-- the most significant half word.
-- The value is taken from an offset and with a number of bit
--
-- Example :
-- > thumbmark <- firstInstruction 11 5
firstInstructionBits :: Int -> Int -> ThumbStreamState Word16
firstInstructionBits off count = do
    (ThumbStream _ w _) <- S.get
    return $ (w `shiftR` off) .&. ((2 ^ count) - 1)


-- | Get the value of a field in a instruction stream
-- Since thumb can be compose of 2 half word this function work on
-- the less significant half word.
-- The value is taken from an offset and with a number of bit
--
-- Example :
-- > thumbmark <- firstInstruction 11 5
secondInstructionBits :: Int -> Int -> ThumbStreamState Word16
secondInstructionBits off count = do
    (ThumbStream _ _ w) <- S.get
    return $ (w `shiftR` off) .&. ((2 ^ count) - 1)
