{-# LANGUAGE FlexibleInstances #-}

module ThumbDecode (
    parseStream
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

instance InstructionStreamState (State ThumbStream) where
    instructionBits = thumbInstructionBits
    nextInstruction = nextThumbInstruction
    parseInstruction = parseThumbInstruction

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
    (ThumbStream _ w _ _) <- S.get
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
    (ThumbStream _ _ w _) <- S.get
    return $ (w `shiftR` off) .&. ((2 ^ count) - 1)

-- | Unify the way we fetch intruction bits
thumbInstructionBits :: Int -> Int -> ThumbStreamState Word32
thumbInstructionBits off size
    | off > 15 = fromIntegral <$> firstInstructionBits off size
    | off <=15 && (off+size) <= 16 = do
        isHalf <- isHalfwordInstruction
        if isHalf
        then fromIntegral <$> firstInstructionBits off size
        else fromIntegral <$> secondInstructionBits off size
    | otherwise = (+) <$> (fromIntegral <$> secondInstructionBits off sizeInFirstWord) <*> 
                          (fromIntegral . (`shiftL` sizeInFirstWord) <$> firstInstructionBits 0 (size - sizeInFirstWord))
            where sizeInFirstWord = 16 - off

-- | Is the current instruction is 16 half word or a full 32 word
-- Retrun True if it is a half word instruction.
isHalfwordInstruction :: ThumbStreamState Bool
isHalfwordInstruction = do
    (ThumbStream _ _ _ b) <- S.get
    return b

-- | Parse regsiter for thumb instruction set where the register is
-- represent using 3 bits
parseThumbRegister :: Int -> ThumbStreamState ArmRegister
parseThumbRegister off = wordToRegister <$> instructionBits off 3

-- | Get the instruction word
instructionWord :: ThumbStreamState Word32
instructionWord = do 
    isHalf <- isHalfwordInstruction
    case isHalf of 
        True -> fromIntegral <$> firstInstructionBits 0 16
        False -> instructionBits 0 32

-- | Thumb entry point decoder
parseThumbInstruction :: ThumbStreamState ArmInstr
parseThumbInstruction = do
    isHalf <- isHalfwordInstruction
    case isHalf of
        True -> parseHalfThumbInstruction
        False -> parseFullThumbInstruction

-- | Parse a thumb 16 bits wide instruction
parseHalfThumbInstruction :: ThumbStreamState ArmInstr
parseHalfThumbInstruction = do
    bitsField <- instructionArrayBits [15,14,13,12,11,10]
    case bitsField of
        [0,0,_,_,_,_] -> parseShiftImmediate
        [0,1,0,0,0,0] -> parseDataProcessing
        [0,1,0,0,0,1] -> parseSpecialDataInstruction
        otherwise     -> return NotParsed

-- | Parse a thumb instruction that have a 32 bit length
parseFullThumbInstruction :: ThumbStreamState ArmInstr
parseFullThumbInstruction = return NotParsed

parseSpecialDataInstruction :: ThumbStreamState ArmInstr
parseSpecialDataInstruction = do
    bitsField <- instructionArrayBits [9,8,7,6]
    case bitsField of
        [0,0,0,0] -> partialInstruction Add <*> parseSpecialRegsiterT1Args   
        [0,0,_,_] -> partialInstruction Add <*> parseSpecialRegsiterT2Args
        [0,1,_,_] -> partialInstruction Cmp <*> parseSpecialRegisterTestT2Args
        [1,0,_,_] -> partialInstruction Mov <*> parseSpecialRegsiterMovT2Args  
        [1,1,0,_] -> partialInstruction Bx  <*> parseSpecialBranchRegisterT1Args
        [1,1,1,_] -> partialInstruction Blx <*> parseSpecialBranchRegisterT1Args   

parseDataProcessing :: ThumbStreamState ArmInstr
parseDataProcessing = do
    bitsField <- instructionArrayBits [9,8,7,6]
    case bitsField of
        [0,0,0,0] -> partialInstruction And <*> parseDataRegisterT1Args   
        [0,0,0,1] -> partialInstruction Eor <*> parseDataRegisterT1Args   
        [0,0,1,0] -> partialInstruction Lsl <*> parseDataRegisterT1Args   
        [0,0,1,1] -> partialInstruction Lsr <*> parseDataRegisterT1Args   
        [0,1,0,0] -> partialInstruction Asr <*> parseDataRegisterT1Args   
        [0,1,0,1] -> partialInstruction Adc <*> parseDataRegisterT1Args   
        [0,1,1,0] -> partialInstruction Sbc <*> parseDataRegisterT1Args   
        [0,1,1,1] -> partialInstruction Ror <*> parseDataRegisterT1Args   
        [1,0,0,0] -> partialInstruction Tst <*> parseDataRegisterTestT1Args  
        [1,0,0,1] -> partialInstruction Rsb <*> parseImmediateT1Arg  
        [1,0,1,0] -> partialInstruction Cmp <*> parseDataRegisterTestT1Args   
        [1,0,1,1] -> partialInstruction Cmn <*> parseDataRegisterTestT1Args
        [1,1,0,0] -> partialInstruction Orr <*> parseDataRegisterT1Args   
        [1,1,0,1] -> partialInstruction Mul <*> parseDataRegisterT1Args   
        [1,1,1,0] -> partialInstruction Bic <*> parseDataRegisterT1Args   
        [1,1,1,1] -> partialInstruction And <*> parseDataRegisterT1Args   
        otherwise -> return Undefined

parseShiftImmediate :: ThumbStreamState ArmInstr
parseShiftImmediate = do
    bitsField <- instructionArrayBits [13,12,11,10,9]
    case bitsField of
        [0,0,0,_,_] -> do
            shiftArg@(ShiftArgs rd rm imm) <- parseShiftImmediateT1Args LSL
            -- There is a special case for encoding a mov
            case imm of
                0 -> partialInstruction Mov <*>
                            (pure $ RegisterToRegisterArgs rd rm)
                otherwise -> partialInstruction Lsl <*>
                            pure shiftArg
        [0,0,1,_,_] -> partialInstruction Lsr <*> parseShiftImmediateT1Args LSR
        [0,1,0,_,_] -> partialInstruction Asr <*> parseShiftImmediateT1Args ASR
        [0,1,1,0,0] -> partialInstruction Add <*> parseRegisterT1Args
        [0,1,1,0,1] -> partialInstruction Sub <*> parseRegisterT1Args
        [0,1,1,1,0] -> partialInstruction Add <*> parseImmediateT1Args
        [0,1,1,1,1] -> partialInstruction Sub <*> parseImmediateT1Args
        [1,0,0,_,_] -> partialInstruction Mov <*> parseImmediateMovT1Args
        [1,0,1,_,_] -> partialInstruction Cmp <*> parseImmediateTestT1Args
        [1,1,0,_,_] -> partialInstruction Add <*> parseImmediate8T1Args
        [1,1,1,_,_] -> partialInstruction Sub <*> parseImmediate8T1Args
        otherwise -> return Undefined

partialInstruction :: InstrClass -> ThumbStreamState (ArgumentsInstruction -> ArmInstr)
partialInstruction cl = ArmInstr <$> instructionWord <*> pure CondAL <*> pure cl <*> pure False

parseShiftImmediateT1Args :: SRType -> ThumbStreamState ArgumentsInstruction
parseShiftImmediateT1Args st = do
    Shift (st,imm) <- decodeImmediateShift st <$> instructionBits 6 5
    ShiftArgs <$> 
        parseThumbRegister 0 <*> 
        parseThumbRegister 3 <*> 
        pure imm
    
parseRegisterT1Args :: ThumbStreamState ArgumentsInstruction
parseRegisterT1Args = RegisterArgs <$>
        parseThumbRegister 3 <*>
        parseThumbRegister 0 <*>
        parseThumbRegister 6 <*>
        -- There is no shift in T1 encoding. So it will be ignore.
        pure LSL <*>
        pure 0            

parseImmediateT1Args :: ThumbStreamState ArgumentsInstruction
parseImmediateT1Args = ImmediateArgs <$> 
        parseThumbRegister 3 <*>
        parseThumbRegister 0 <*>
        instructionBits 6 3

parseImmediateMovT1Args :: ThumbStreamState ArgumentsInstruction
parseImmediateMovT1Args = ImmediateMovArgs <$>
        parseThumbRegister 8 <*>
        instructionBits 0 8

parseImmediateTestT1Args :: ThumbStreamState ArgumentsInstruction
parseImmediateTestT1Args = ImmediateTestArgs <$>
        parseThumbRegister 8 <*>
        instructionBits 0 8

parseImmediate8T1Args :: ThumbStreamState ArgumentsInstruction
parseImmediate8T1Args = ImmediateArgs <$>
        parseThumbRegister 8 <*>
        parseThumbRegister 8 <*>
        instructionBits 0 8

parseDataRegisterT1Args :: ThumbStreamState ArgumentsInstruction
parseDataRegisterT1Args = RegisterArgs <$>
        parseThumbRegister 0 <*>
        parseThumbRegister 0 <*>
        parseThumbRegister 3 <*>
        -- There is no shift in T1 encoding. So it will be ignore.
        pure LSL <*>
        pure 0

parseDataRegisterTestT1Args :: ThumbStreamState ArgumentsInstruction
parseDataRegisterTestT1Args = RegisterTestArgs <$>
        parseThumbRegister 0 <*>
        parseThumbRegister 3 <*>
        -- There is no shift in T1 encoding. So it will be ignore.
        pure LSL <*>
        pure 0

parseSpecialRegisterTestT2Args :: ThumbStreamState ArgumentsInstruction
parseSpecialRegisterTestT2Args = do     
    n <- (+) <$> ((8*) <$> instructionBits 7 1) <*> instructionBits 0 3
    RegisterTestArgs <$>
        (pure $ wordToRegister n) <*>
        parseRegister 3 <*>
        -- There is no shift in T1 encoding. So it will be ignore.
        pure LSL <*>
        pure 0

parseImmediateT1Arg :: ThumbStreamState ArgumentsInstruction
parseImmediateT1Arg = ImmediateArgs <$>
        parseThumbRegister 3 <*>
        parseThumbRegister 0 <*>
        pure 0


parseSpecialRegsiterT1Args :: ThumbStreamState ArgumentsInstruction
parseSpecialRegsiterT1Args = RegisterArgs <$> 
        parseThumbRegister 3 <*>
        parseThumbRegister 0 <*>
        parseThumbRegister 6 <*>
        -- There is no shift in T1 encoding. So it will be ignore.
        pure LSL <*>
        pure 0

parseSpecialRegsiterT2Args :: ThumbStreamState ArgumentsInstruction
parseSpecialRegsiterT2Args = do
    dn <- (+) <$> ((8*) <$> instructionBits 7 1) <*> instructionBits 0 3
    RegisterArgs <$> 
        (pure $ wordToRegister dn) <*>
        (pure $ wordToRegister dn) <*>
        parseRegister 3 <*>
        -- There is no shift in T1 encoding. So it will be ignore.
        pure LSL <*>
        pure 0

parseSpecialRegsiterMovT2Args :: ThumbStreamState ArgumentsInstruction
parseSpecialRegsiterMovT2Args = do
    d <- (+) <$> ((8*) <$> instructionBits 7 1) <*> instructionBits 0 3
    RegisterToRegisterArgs <$> 
        (pure $ wordToRegister d) <*>
        parseRegister 3

parseSpecialBranchRegisterT1Args :: ThumbStreamState ArgumentsInstruction
parseSpecialBranchRegisterT1Args = BranchExchangeArgs <$> parseRegister 3

parseStream :: ByteString -> [ArmInstr]
parseStream s = fst (runState (parseInstrStream 50) (ThumbStream s 0 0 False))

