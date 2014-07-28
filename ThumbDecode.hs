{-# LANGUAGE FlexibleInstances #-}

module ThumbDecode (
) where

import Internal.Type
import Data.Binary
import qualified Data.Binary.Get as Bin
import Data.Binary.Bits
import Data.Bits 
import Data.ByteString
import Control.Monad
import Control.Monad.State.Lazy
import Control.Applicative
import qualified Control.Monad.State.Lazy as S
import Text.Printf
import Data.Tuple.All

data ThumbStream = ThumbStream ByteString Word16 Word16 Bool

type ThumbStreamState a = S.State ThumbStream a

-- | Advance to the next instruction
nextThumbInstruction :: ThumbStreamState ()
nextThumbInstruction = do
    (ThumbStream b ph pl _) <- S.get
    case Bin.pushChunk (Bin.runGetIncremental Bin.getWord16le) b of
        Bin.Done resultS _ word -> do
            S.put $  ThumbStream resultS word pl True
            instMark <- firstInstructionBits 11 5
            if instMark == 29 || instMark == 30 || instMark == 31
            then case Bin.pushChunk (Bin.runGetIncremental Bin.getWord16le) resultS of
                Bin.Done resultL _ wordLo -> do
                    S.put $ ThumbStream resultL word wordLo False
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

-- | Unify the way we fetch intruction bits
instance InstructionStreamState (State ThumbStream) where
    instructionBits off size
        | off > 15                    = fromIntegral <$> firstInstructionBits off size
        | off <=15 && (off+size) > 16 = do ... fromIntegral <$> secondInstructionBits off size 
        | otherwise                   = (+) <$> (fromIntegral <$> secondInstructionBits off sizeInFirstWord) <*> 
                                                (fromIntegral . (`shiftL` sizeInFirstWord) <$> firstInstructionBits 0 (size - sizeInFirstWord))
            where sizeInFirstWord = 16 - off

-- | Is the current instruction is 16 half word or a full 32 word
-- Retrun True if it is a half word instruction.
isHalfwordInstruction :: ArmStreamState Bool
isHalfwordInstruction = do
    (ArmStreamState _ _ _ b) <- get
    return b

instructionWord :: ThumbStreamState Word32
instructionWord = do 
    isHalf <- isHalfwordInstruction
    case isHalf of 
        True -> fromIntegral <$> firstInstruction
        False -> instructionBits 0 32

-- | Thumb entry point decoder
parseThumbInstruction :: ThumbStreamState ArmInstr
parseThumbInstruction = do
    isHalf <- isHalfwordInstruction
    case isHalf of
        True -> parseHalfThumbInstruction
        False -> parseFullThumbInstruction

parseHalfThumbInstruction :: ThumbStreamState ArmInstr
parseHalfThumbInstruction = do
    bitsField <- instructionArrayBits [15,14,13,12,11,10]
    case bitsField of
        [0,0,_,_,_,_] -> parseShiftImmediate
        otherwise     -> return NotParsed

parseShiftImmediate :: ThumbStreamState ArmInstr
parseShiftImmediate = do
    bitsField <- instructionArrayBits [13,12,11,10,9]
    case bitsField of
        [0,0,0,_,_] -> ArmInstr <$>
                            instructionWord <*> 
                            pure CondAL <*>
                            pure Lsl <*>
                            pure False <*>
                            parseShiftImmediateT1Args

parseShiftImmediateT1Args :: ArmStreamState ArgumentsInstruction
parseShiftImmediateT1Args = ShiftArgs <$> parseRegister 
    
                            
