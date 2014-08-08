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
    | off > 15 = fromIntegral <$> firstInstructionBits (off-16) size
    | off <=15 && (off+size) <= 16 = do
        isHalf <- isHalfwordInstruction
        if isHalf
        then fromIntegral <$> firstInstructionBits off size
        else fromIntegral <$> secondInstructionBits off size
    | otherwise = (+) <$> (fromIntegral <$> secondInstructionBits off sizeInFirstWord) <*> 
                          ((`shiftL` sizeInFirstWord) . fromIntegral <$> firstInstructionBits 0 (size - sizeInFirstWord))
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
        [0,1,0,0,0,0] -> parseHalfDataProcessing
        [0,1,0,0,0,1] -> parseSpecialDataInstruction
        [0,1,0,0,1,_] -> partialInstruction Ldr <*> parseLoadLiteralT1Args
        [0,1,0,1,_,_] -> parseLoadStoreSingleDataItem
        [0,1,1,_,_,_] -> parseLoadStoreSingleDataItem
        [1,0,0,_,_,_] -> parseLoadStoreSingleDataItem
        [1,0,1,1,_,_] -> parseHalfMiscellaneous
        [1,1,0,0,0,_] -> partialInstruction Stm <*> parseLoadStoreMultipleT1Args 
        [1,1,0,0,1,_] -> partialInstruction Ldm <*> parseLoadStoreMultipleT1Args 
        [1,1,0,1,_,_] -> parseConditionalBranchAndSupervisorCall
        [1,1,1,0,0,_] -> partialInstruction B <*> parseBranchImmediateT2Args
        otherwise     -> return NotParsed

-- | Parse a thumb instruction that have a 32 bit length
-- Section A6.3 of the reference manual.
parseFullThumbInstruction :: ThumbStreamState ArmInstr
parseFullThumbInstruction = do
    bitsField <- instructionArrayBits ((Prelude.map (+16) [12,11,10,9,8,7,6,5,4]) ++ [15])
    case bitsField of
        [0,1, 0,0,_,_,0,_,_, _] -> parseLoadStoreMultiple
        [0,1, 0,1,_,_,_,_,_, _] -> parseDataProcessingRegister
        [1,0, _,0,_,_,_,_,_, 0] -> parseDataProcessingModifiedImmediate
        [1,0, _,1,_,_,_,_,_, 0] -> parseDataProcessingPlainImmediate
        [1,0, _,_,_,_,_,_,_, 1] -> parseBranchAndMiscellaneousControl
        [1,1, 0,0,_,_,0,0,1, _] -> parseLoadByte
        [1,1, 0,0,_,_,1,0,1, _] -> parseLoadWord
        otherwise -> Undefined <$> instructionWord 

-- | Branch and miscellaneous control
-- Section A6.3.4
parseBranchAndMiscellaneousControl :: ThumbStreamState ArmInstr
parseBranchAndMiscellaneousControl = do
    bitsField <- instructionArrayBits [14,13,12,7,6,5,4,3,2,1,0,26,25,24,23,22,21,20,11,10,9,8]
    case bitsField of 
        [0,0,0, _,_,_,_,_,_,_,_, 1,1,1,1,1,1,0, _,_,_,_] -> partialInstruction Hvc <*> pure NullArgs
        [0,0,0, _,_,_,_,_,_,_,_, 1,1,1,1,1,1,1, _,_,_,_] -> partialInstruction Smc <*> pure NullArgs
        [0,1,0, _,_,_,_,_,_,_,_, 1,1,1,1,1,1,1, _,_,_,_] -> partialInstruction Udf <*> pure NullArgs
        [0,_,1, _,_,_,_,_,_,_,_, 1,1,1,1,1,1,1, _,_,_,_] -> partialInstruction B <*> pure NullArgs
        otherwise -> Undefined <$> instructionWord

-- | Load byte, memory hints
-- Section A6.3.9 of the reference manual.
parseLoadByte :: ThumbStreamState ArmInstr
parseLoadByte = do
    bitsField <- instructionArrayBits [24,23,11,10,9,8,7,6,19,18,17,16,15,14,13,12]
    case bitsField of
        [0,_, _,_,_,_,_,_, 1,1,1,1, 1,1,1,1] -> partialInstruction Pld <*> pure NullArgs
        [0,_, _,_,_,_,_,_, 1,1,1,1, _,_,_,_] -> partialInstruction Ldrb <*> parseLoadLiteralT2Args
        [0,0, 0,0,0,0,0,0, _,_,_,_, 1,1,1,1] -> partialInstruction Pldw <*> pure NullArgs
        [0,0, 0,0,0,0,0,0, _,_,_,_, _,_,_,_] -> partialInstruction Ldrb <*> parseLoadStoreRegisterT2Args
        [0,0, 1,1,0,0,_,_, _,_,_,_, 1,1,1,1] -> partialInstruction Pldw <*> pure NullArgs
        [0,0, 1,_,_,1,_,_, _,_,_,_, _,_,_,_] -> partialInstruction Ldrb <*> parseLoadStoreImmediateT4Args
        otherwise -> Undefined <$> instructionWord

-- | Data processing plain binary immediate
-- Section A6.3.3 of the reference manual.
parseDataProcessingPlainImmediate :: ThumbStreamState ArmInstr
parseDataProcessingPlainImmediate = do
    bitsField <- instructionArrayBits [8,7,6,5,4,3,2,1,0]
    case bitsField of 
        [0,0,0,0,0, 1,1,1,1] -> partialInstruction Adr <*> parseImmediateRelativeTXArgs True 
        [0,0,0,0,0, _,_,_,_] -> partialInstruction Add <*> parseImmediatePlainT4Args
        [0,0,1,0,0, _,_,_,_] -> partialInstruction Mov <*> parseImmediateMovPlainT3Args
        [0,1,0,1,0, 1,1,1,1] -> partialInstruction Adr <*> parseImmediateRelativeTXArgs False
        [0,1,0,1,0, _,_,_,_] -> partialInstruction Sub <*> parseImmediatePlainT4Args
        [0,1,1,0,0, _,_,_,_] -> partialInstruction Movt <*> parseImmediateMovPlainT3Args
        otherwise -> Undefined <$> instructionWord

-- | Load and store multiple instruction decoding
-- Section A6.3.5 of the reference manual.
parseLoadStoreMultiple :: ThumbStreamState ArmInstr
parseLoadStoreMultiple = do
    bitsField <- instructionArrayBits  (Prelude.map (+16) [8,7,4,5,3,2,1,0])
    case bitsField of
        [0,0, 0, _,_,_,_,_] -> partialInstruction Srsdb <*> parseStoreReturnStateT1Args 
        [0,0, 1, _,_,_,_,_] -> partialInstruction Rfedb <*> parseReturnFromExceptionT1Args
        [0,1, 0, _,_,_,_,_] -> partialInstruction Stm <*> parseStoreMultipleT2Args
        [0,1, 1, 1,1,1,0,1] -> partialInstruction Pop <*> parsePopRegisterListT2Args
        [0,1, 1, _,_,_,_,_] -> partialInstruction Ldm <*> parseLoadMultipleT2Args
        [1,0, 0, 1,1,1,0,1] -> partialInstruction Push <*> parsePushRegisterListT2Args
        [1,0, 0, _,_,_,_,_] -> partialInstruction Stmdb <*> parseStoreMultipleT2Args
        [1,0, 1, _,_,_,_,_] -> partialInstruction Ldmdb <*> parseLoadMultipleT2Args
        [1,1, 0, _,_,_,_,_] -> partialInstruction Srsia <*> parseStoreReturnStateT1Args 
        [1,1, 1, _,_,_,_,_] -> partialInstruction Rfeia <*> parseReturnFromExceptionT1Args
        otherwise -> Undefined <$> instructionWord

-- | Load word instruction decoding
-- Section A6.3.7 of the reference manual.
parseLoadWord :: ThumbStreamState ArmInstr
parseLoadWord = do
    bitsField <- instructionArrayBits [24,23,11,10,9,8,7,6,19,18,17,16] 
    case bitsField of
        [0,_, _,_,_,_,_,_, 1,1,1,1] -> partialInstruction Ldr <*> parseLoadLiteralT2Args
        [0,0, 0,0,0,0,0,0, _,_,_,_] -> partialInstruction Ldr <*> parseLoadStoreRegisterT2Args
        [0,0, 1,_,_,1,_,_, _,_,_,_] -> partialInstruction Ldr <*> parseLoadStoreImmediateT3Args
        [0,0, 1,1,0,0,_,_, _,_,_,_] -> partialInstruction Ldr <*> parseLoadStoreImmediateT3Args
        [0,1, _,_,_,_,_,_, _,_,_,_] -> partialInstruction Ldr <*> parseLoadStoreImmediateT3Args
        [0,0, 1,1,1,0,_,_, _,_,_,_] -> partialInstruction Ldrt <*> parseLoadStoreImmediateUnprivilegedT1Args
        otherwise -> Undefined <$> instructionWord

-- | Data processing encoding 32bit thumb.
-- Section A6.3.1 of the reference manual.
parseDataProcessingModifiedImmediate :: ThumbStreamState ArmInstr
parseDataProcessingModifiedImmediate = do
    bitsField <- instructionArrayBits $ (Prelude.map (+16) [8,7,6,5,3,2,1,0]) ++ [11,10,9,8,20]
    case bitsField of 
        [0,0,0,0, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Tst 20 <*> parseImmediate12TestT1Args
        [0,0,0,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags And 20 <*> parseImmediate12T1Args
        [0,0,0,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Bic 20 <*> parseImmediate12T1Args
        [0,0,1,0, 1,1,1,1, _,_,_,_,_] -> partialInstructionWithFlags Mov 20 <*> parseImmediate12MovT2Args
        [0,0,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Orr 20 <*> parseImmediate12T1Args
        [0,0,1,1, 1,1,1,1, _,_,_,_,_] -> partialInstructionWithFlags Mvn 20 <*> parseImmediate12MovT2Args
        [0,0,1,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Orn 20 <*> parseImmediate12T1Args
        [0,1,0,0, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Teq 20 <*> parseImmediate12TestT1Args
        [0,1,0,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Eor 20 <*> parseImmediate12T1Args
        [1,0,0,0, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Cmn 20 <*> parseImmediate12TestT1Args
        [1,0,0,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Add 20 <*> parseImmediate12T1Args
        [1,0,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Adc 20 <*> parseImmediate12T1Args
        [1,0,1,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Sbc 20 <*> parseImmediate12T1Args
        [1,1,0,1, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Cmp 20 <*> parseImmediate12TestT1Args
        [1,1,0,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Sub 20 <*> parseImmediate12T1Args
        [1,1,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Rsb 20 <*> parseImmediate12T1Args
        otherwise -> Undefined <$> instructionWord

-- | Decode data processing 32bit shift register
-- Section A6.3.11 of the reference manual.
parseDataProcessingRegister :: ThumbStreamState ArmInstr
parseDataProcessingRegister = do
    bitsField <- instructionArrayBits ((Prelude.map (+16) [8,7,6,5,3,2,1,0]) ++ [11,10,9,8,20])
    case bitsField of
        [0,0,0,0, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Tst 20 <*> parseRegisterTestT2Args
        [0,0,0,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags And 20 <*> parseRegisterT2Args
        [0,0,0,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Bic 20 <*> parseRegisterT2Args
        [0,0,1,0, 1,1,1,1, _,_,_,_,_] -> parseMovAndImmediateShift
        [0,0,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Orr 20 <*> parseRegisterT2Args
        [0,0,1,1, 1,1,1,1, _,_,_,_,_] -> partialInstructionWithFlags Mvn 20 <*> parseRegisterMvnT2Args
        [0,0,1,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Orn 20 <*> parseRegisterT2Args
        [0,1,0,0, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Teq 20 <*> parseRegisterTestT2Args
        [0,1,0,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Eor 20 <*> parseRegisterT2Args
        [0,1,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Pkh 20 <*> pure NullArgs
        [1,0,0,0, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Cmn 20 <*> parseRegisterTestT2Args
        [1,0,0,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Add 20 <*> parseRegisterT2Args
        [1,0,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Adc 20 <*> parseRegisterT2Args
        [1,0,1,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Sbc 20 <*> parseRegisterT2Args
        [1,1,0,1, _,_,_,_, 1,1,1,1,1] -> partialInstructionWithFlags Cmp 20 <*> parseRegisterTestT2Args
        [1,1,0,1, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Sub 20 <*> parseRegisterT2Args
        [1,1,1,0, _,_,_,_, _,_,_,_,_] -> partialInstructionWithFlags Rsb 20 <*> parseRegisterT2Args
        otherwise -> Undefined <$> instructionWord 

-- | Special  decode case for mov instruction 
-- Follow up on the A6.3.11 section from the reference manual.
parseMovAndImmediateShift :: ThumbStreamState ArmInstr
parseMovAndImmediateShift = do
    bitsField <- instructionArrayBits [5,4,14,13,12,7,6]
    case bitsField of
        [0,0, 0,0,0,0,0] -> partialInstructionWithFlags Mov 20 <*> parseRegisterMovT3Args
        [0,0, _,_,_,_,_] -> partialInstructionWithFlags Lsl 20 <*> parseImmediateTestT2Args
        [0,1, _,_,_,_,_] -> partialInstructionWithFlags Lsr 20 <*> parseImmediateTestT2Args
        [1,0, _,_,_,_,_] -> partialInstructionWithFlags Asr 20 <*> parseImmediateTestT2Args
        [1,1, 0,0,0,0,0] -> partialInstructionWithFlags Rrx 20 <*> parseRegisterMovT3Args
        [1,1, _,_,_,_,_] -> partialInstructionWithFlags Ror 20 <*> parseImmediateTestT2Args
        


{------------------------------------------------------------------------------
 - Docoding table for 16 bit thumb instruction
 -----------------------------------------------------------------------------}

parseConditionalBranchAndSupervisorCall :: ThumbStreamState ArmInstr
parseConditionalBranchAndSupervisorCall = do
    bitsField <- instructionArrayBits [11,10,9,8]
    case bitsField of 
        [1,1,1,0] -> partialInstruction Udf <*> pure NoArgs
        [1,1,1,1] -> partialInstruction Svc <*> parseSupervisorImmediateT1Args
        otherwise -> partialInstructionWithCondition B (parseCondAt 8) <*> parseBranchImmediateT1Args

parseHalfMiscellaneous :: ThumbStreamState ArmInstr
parseHalfMiscellaneous = do
    bitsField <- instructionArrayBits [11,10,9,8,7,6,5]
    case bitsField of
        [0,0,0,0,0,_,_] -> partialInstruction Add <*> parseMiscArithmeticT2Args
        [0,0,0,0,1,_,_] -> partialInstruction Sub <*> parseMiscArithmeticT2Args
        [0,0,0,1,_,_,_] -> partialInstruction Cbz <*> parseCompareBranchT1Args
        [0,0,1,0,0,0,_] -> partialInstruction Sxth <*> parseCompareBranchT1Args
        [0,0,1,0,0,1,_] -> partialInstruction Sxtb <*> parseCompareBranchT1Args
        [0,0,1,0,1,0,_] -> partialInstruction Uxth <*> parseCompareBranchT1Args
        [0,0,1,0,1,1,_] -> partialInstruction Uxtb <*> parseCompareBranchT1Args
        [0,0,1,1,_,_,_] -> partialInstruction Cbz <*> parseCompareBranchT1Args
        [0,1,0,_,_,_,_] -> partialInstruction Push <*> parsePushRegisterListT1Args
        [0,1,1,0,0,1,0] -> partialInstruction Setend <*> parseSetEndianessT1Args
        [0,1,1,0,0,1,1] -> partialInstruction Cps <*> parseSetEndianessT1Args
        [1,0,0,1,_,_,_] -> partialInstruction Cbnz <*> parseCompareBranchT1Args
        [1,0,1,0,0,0,_] -> partialInstruction Rev <*> parseRegisterToRegisterT1Args
        [1,0,1,0,0,1,_] -> partialInstruction Rev16 <*> parseRegisterToRegisterT1Args
        [1,0,1,0,1,1,_] -> partialInstruction Revsh <*> parseRegisterToRegisterT1Args
        [1,0,1,1,_,_,_] -> partialInstruction Cbnz <*> parseCompareBranchT1Args
        [1,1,0,_,_,_,_] -> partialInstruction Pop <*> parsePopRegisterListT1Args
        otherwise -> Undefined <$> instructionWord

-- | Load and store a single data
-- Section A6.2.4 of the reference manual.
parseLoadStoreSingleDataItem :: ThumbStreamState ArmInstr
parseLoadStoreSingleDataItem = do
    bitsField <- instructionArrayBits [15,14,13,12,11,10,9]
    case bitsField of
        [0,1,0,1, 0,0,0] -> partialInstruction Str <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 0,0,1] -> partialInstruction Strh <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 0,1,0] -> partialInstruction Strb <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 0,1,1] -> partialInstruction Ldrsb <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 1,0,0] -> partialInstruction Ldr <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 1,0,1] -> partialInstruction Ldrh <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 1,1,0] -> partialInstruction Ldrb <*> parseLoadStoreRegisterT1Args
        [0,1,0,1, 1,1,1] -> partialInstruction Ldrsh <*> parseLoadStoreRegisterT1Args
        [0,1,1,0, 0,_,_] -> partialInstruction Str <*> parseLoadStoreImmediateT1Args
        [0,1,1,0, 1,_,_] -> partialInstruction Ldr <*> parseLoadStoreImmediateT1Args
        [0,1,1,1, 0,_,_] -> partialInstruction Strb <*> parseLoadStoreImmediateT1Args
        [0,1,1,1, 1,_,_] -> partialInstruction Ldrb <*> parseLoadStoreImmediateT1Args
        [1,0,0,0, 0,_,_] -> partialInstruction Strh <*> parseLoadStoreImmediateT1Args
        [1,0,0,0, 1,_,_] -> partialInstruction Ldrh <*> parseLoadStoreImmediateT1Args
        [1,0,0,1, 0,_,_] -> partialInstruction Str <*> parseLoadStoreImmediateT2Args
        [1,0,0,1, 1,_,_] -> partialInstruction Ldr <*> parseLoadStoreImmediateT2Args
        otherwise -> Undefined <$> instructionWord

parseSpecialDataInstruction :: ThumbStreamState ArmInstr
parseSpecialDataInstruction = do
    bitsField <- instructionArrayBits [9,8,7,6]
    case bitsField of
        [0,0,_,_] -> partialInstruction Add <*> parseSpecialRegsiterT2Args
        [0,1,_,_] -> partialInstruction Cmp <*> parseSpecialRegisterTestT2Args
        [1,0,_,_] -> partialInstruction Mov <*> parseSpecialRegsiterMovT2Args  
        [1,1,0,_] -> partialInstruction Bx  <*> parseSpecialBranchRegisterT1Args
        [1,1,1,_] -> partialInstruction Blx <*> parseSpecialBranchRegisterT1Args
        otherwise -> Undefined <$> instructionWord

parseHalfDataProcessing :: ThumbStreamState ArmInstr
parseHalfDataProcessing = do
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
        otherwise -> Undefined <$> instructionWord

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
        otherwise -> Undefined <$> instructionWord

partialInstruction :: InstrClass -> ThumbStreamState (ArgumentsInstruction -> ArmInstr)
partialInstruction cl = ArmInstr <$> instructionWord <*> pure CondAL <*> pure cl <*> pure False

partialInstructionWithFlags :: InstrClass -> Int -> ThumbStreamState (ArgumentsInstruction -> ArmInstr)
partialInstructionWithFlags cl off = ArmInstr <$> instructionWord <*> pure CondAL <*> pure cl <*> instructionFlag off

partialInstructionWithCondition :: InstrClass -> ThumbStreamState Cond -> ThumbStreamState (ArgumentsInstruction -> ArmInstr)
partialInstructionWithCondition cl cond = ArmInstr <$> instructionWord <*> cond <*> pure cl <*> pure False

{------------------------------------------------------------------------------
 - Argument parsing function
 -----------------------------------------------------------------------------}

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

parseLoadLiteralT1Args :: ThumbStreamState ArgumentsInstruction
parseLoadLiteralT1Args = LoadLiteralArgs <$> 
        parseThumbRegister 8 <*> 
        ((`rotateR` 2) <$> (instructionBits 0 8)) <*>
        pure True

parseLoadLiteralT2Args :: ThumbStreamState ArgumentsInstruction
parseLoadLiteralT2Args = LoadLiteralArgs <$>
        parseRegister 12 <*>
        instructionBits 0 12 <*>
        instructionFlag 24

parseLoadStoreRegisterT1Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreRegisterT1Args = LoadStoreRegisterArgs <$>
    parseThumbRegister 6 <*>
    parseThumbRegister 3 <*>
    parseThumbRegister 0 <*>
    pure 0

parseLoadStoreRegisterT2Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreRegisterT2Args = LoadStoreRegisterArgs <$>
    parseRegister 0 <*>
    parseRegister 16 <*>
    parseRegister 12 <*>
    instructionBits 4 2

parseLoadStoreImmediateT1Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreImmediateT1Args = LoadStoreImmediateArgs <$>
    parseThumbRegister 3 <*>
    parseThumbRegister 0 <*>
    ((`shiftL` 2) <$> instructionBits 6 5) <*>
    pure False <*>
    pure True <*>
    pure True

parseLoadStoreImmediateT2Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreImmediateT2Args = LoadStoreImmediateArgs <$>
    pure SP <*>
    parseThumbRegister 8 <*>
    ((`shiftL` 2) <$> instructionBits 0 8) <*>
    pure False <*>
    pure True <*>
    pure True

parseLoadStoreImmediateT3Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreImmediateT3Args = LoadStoreImmediateArgs <$>
    parseRegister 16 <*>
    parseRegister 12 <*>
    instructionBits 0 12 <*>
    pure False <*>
    pure True <*>
    pure True

parseLoadStoreImmediateT4Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreImmediateT4Args = LoadStoreImmediateArgs <$>
    parseRegister 16 <*>
    parseRegister 12 <*>
    instructionBits 0 8 <*>
    instructionFlag 8 <*>
    instructionFlag 10 <*>
    instructionFlag 9

parseLoadStoreImmediateUnprivilegedT1Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreImmediateUnprivilegedT1Args = LoadStoreImmediateArgs <$>
    parseRegister 16 <*>
    parseRegister 12 <*>
    instructionBits 0 8 <*>
    pure False <*>
    pure True <*>
    pure True

parseSupervisorImmediateT1Args :: ThumbStreamState ArgumentsInstruction
parseSupervisorImmediateT1Args = BranchArgs <$> instructionBits 0 8

parseBranchImmediateT1Args :: ThumbStreamState ArgumentsInstruction
parseBranchImmediateT1Args = BranchArgs <$> 
    ((`shiftL` 1) <$> instructionSignedExtendBits 0 8)

parseBranchImmediateT2Args :: ThumbStreamState ArgumentsInstruction
parseBranchImmediateT2Args = BranchArgs <$> 
    ((`shiftL` 1) <$> instructionSignedExtendBits 0 11)

parseSpecialBranchRegisterT1Args :: ThumbStreamState ArgumentsInstruction
parseSpecialBranchRegisterT1Args = BranchExchangeArgs <$> parseRegister 3

parseAddT1Args :: ThumbStreamState ArgumentsInstruction
parseAddT1Args = ImmediateArgs SP <$>
    parseThumbRegister 8 <*>
    ((`shiftL` 2) <$> instructionBits 0 8)

parseMiscArithmeticT2Args :: ThumbStreamState ArgumentsInstruction
parseMiscArithmeticT2Args = ImmediateArgs SP SP <$> ((`shiftL` 2) <$> instructionBits 0 7)

parseExtractT1Args :: ThumbStreamState ArgumentsInstruction
parseExtractT1Args = ExtractArgs <$>
    parseThumbRegister 3 <*>
    parseThumbRegister 0 <*>
    -- Not used in T1
    pure 0

parseCompareBranchT1Args :: ThumbStreamState ArgumentsInstruction
parseCompareBranchT1Args = CompareBranchArgs <$>
    parseThumbRegister 0 <*>
    ((*2) <$> ((+) <$> ((`shiftL` 5) <$> instructionBits 11 1) <*> instructionBits 3 5))

parsePushRegisterListT1Args :: ThumbStreamState ArgumentsInstruction
parsePushRegisterListT1Args = do
    m <- instructionBits 8 1
    reglist <- instructionArrayBits [7,6,5,4,3,2,1,0]
    return $ RegisterListArgs $ decodeRegisterList $ 0:m:0:0:0:0:0:0:reglist

parsePopRegisterListT1Args :: ThumbStreamState ArgumentsInstruction
parsePopRegisterListT1Args = do
    p <- instructionBits 8 1
    reglist <- instructionArrayBits [7,6,5,4,3,2,1,0]
    return $ RegisterListArgs $ decodeRegisterList $ p:0:0:0:0:0:0:0:reglist

parsePopRegisterListT2Args :: ThumbStreamState ArgumentsInstruction
parsePopRegisterListT2Args = RegisterListArgs <$>
        ((\p m t -> decodeRegisterList $ p:m:0:t) <$> 
            instructionBits 15 1 <*> 
            instructionBits 14 1 <*> 
            instructionArrayBits [12,11,10,9,8,7,6,5,4,3,2,1,0]) 

parsePushRegisterListT2Args :: ThumbStreamState ArgumentsInstruction
parsePushRegisterListT2Args = RegisterListArgs <$>
        ((\m t -> decodeRegisterList $ 0:m:0:t) <$> 
            instructionBits 14 1 <*> 
            instructionArrayBits [12,11,10,9,8,7,6,5,4,3,2,1,0]) 
parseSetEndianessT1Args :: ThumbStreamState ArgumentsInstruction
parseSetEndianessT1Args = SettingEndiannessArgs <$> (wordToEndianState <$> instructionBits 3 1) 

parseChangeProcessorStateT1Args :: ThumbStreamState ArgumentsInstruction
parseChangeProcessorStateT1Args = ChangeProcessorStateArgs <$> 
        (not <$> instructionFlag 4) <*>
        instructionFlag 2 <*>
        instructionFlag 1 <*>
        instructionFlag 0 <*>
        pure False <*>
        pure 0 

parseRegisterToRegisterT1Args :: ThumbStreamState ArgumentsInstruction
parseRegisterToRegisterT1Args = RegisterToRegisterArgs <$>
        parseThumbRegister 0 <*>
        parseThumbRegister 3

parseLoadStoreMultipleT1Args :: ThumbStreamState ArgumentsInstruction
parseLoadStoreMultipleT1Args = LoadAndStoreRegisterListArgs <$>
        parseThumbRegister 8 <*>
        pure True <*>
        (decodeRegisterList . ([0,0,0,0,0,0,0,0]++) <$> instructionArrayBits [7,6,5,4,3,2,1,0])

parseStoreMultipleT2Args :: ThumbStreamState ArgumentsInstruction
parseStoreMultipleT2Args = LoadAndStoreRegisterListArgs <$>
        parseRegister 16 <*>    
        instructionFlag 21 <*> pure [R0]
        --((\m t -> decodeRegisterList $ 0:m:0:t) <$> instructionBits 14 1 <*> instructionArrayBits [12,11,10,9,8,7,6,4,3,2,1,0])

parseLoadMultipleT2Args :: ThumbStreamState ArgumentsInstruction
parseLoadMultipleT2Args = LoadAndStoreRegisterListArgs <$>
        parseRegister 16 <*>
        instructionFlag 21 <*>
        ((\p m t -> decodeRegisterList $ p:m:0:t) <$> 
            instructionBits 15 1 <*>
            instructionBits 14 1 <*>
            instructionArrayBits [12,11,10,9,8,7,6,4,3,2,1,0])

parseRegisterTestT2Args :: ThumbStreamState ArgumentsInstruction
parseRegisterTestT2Args = RegisterTestArgs <$> 
        parseRegister 16 <*> 
        parseRegister 0 <*>
        (wordToSRType <$> instructionBits 4 2) <*>
        decodeImmediate5T2

parseRegisterT2Args :: ThumbStreamState ArgumentsInstruction
parseRegisterT2Args = RegisterArgs <$> 
        parseRegister 16 <*> 
        parseRegister 8 <*> 
        parseRegister 0 <*>
        (wordToSRType <$> instructionBits 4 2) <*>
        decodeImmediate5T2

parseRegisterMovT3Args :: ThumbStreamState ArgumentsInstruction
parseRegisterMovT3Args = RegisterToRegisterArgs <$>
        parseRegister 8 <*>
        parseRegister 0

parseImmediateTestT2Args :: ThumbStreamState ArgumentsInstruction
parseImmediateTestT2Args = ImmediateArgs <$>
        parseRegister 0 <*>
        parseRegister 8 <*>
        decodeImmediate5T2

parseRegisterMvnT2Args :: ThumbStreamState ArgumentsInstruction
parseRegisterMvnT2Args = RegisterMvnArgs <$>
        parseRegister 8 <*> 
        parseRegister 0 <*>
        (wordToSRType <$> instructionBits 4 2) <*>
        decodeImmediate5T2

parseImmediate12TestT1Args :: ThumbStreamState ArgumentsInstruction
parseImmediate12TestT1Args = ImmediateTestArgs <$> 
        parseRegister 16 <*>
        decodeImmediate12T1

parseImmediate12MovT2Args :: ThumbStreamState ArgumentsInstruction
parseImmediate12MovT2Args = ImmediateMovArgs <$> 
        parseRegister 8 <*>
        decodeImmediate12T1

parseImmediate12T1Args :: ThumbStreamState ArgumentsInstruction
parseImmediate12T1Args = ImmediateArgs <$> 
        parseRegister 16 <*>
        parseRegister 8 <*>
        decodeImmediate12T1

parseStoreReturnStateT1Args :: ThumbStreamState ArgumentsInstruction
parseStoreReturnStateT1Args = StoreReturnStateArgs <$>
        instructionFlag 21 <*>
        instructionBits 0 5

parseReturnFromExceptionT1Args :: ThumbStreamState ArgumentsInstruction
parseReturnFromExceptionT1Args = ReturnArgs <$>
        parseRegister 16 <*>
        instructionFlag 21

parseImmediateRelativeTXArgs :: Bool -> ThumbStreamState ArgumentsInstruction
parseImmediateRelativeTXArgs adding = ImmediateRelativeArgs <$>
        parseRegister 8 <*>
        decodeImmediate12T2 <*>
        pure adding
       
parseImmediatePlainT4Args :: ThumbStreamState ArgumentsInstruction
parseImmediatePlainT4Args = ImmediateArgs <$>
        parseRegister 16 <*>
        parseRegister 8 <*>
        decodeImmediate12T2
 
parseImmediateMovPlainT3Args :: ThumbStreamState ArgumentsInstruction
parseImmediateMovPlainT3Args = ImmediateMovArgs <$>
        parseRegister 8 <*>
        decodeImmediate16T1

parseBranchImmediateT4Args :: ThumbStreamState ArgumentsInstruction
parseBranchImmediateT4Args = 

decodeImmediate12T2 :: ThumbStreamState Word32
decodeImmediate12T2 = do
    imm8 <- instructionBits 0 8
    imm3 <- (`shiftL` 8) <$> instructionBits 12 3
    i <- (`shiftL` 11) <$> instructionBits 26 1
    return $ imm8 + imm3 + i

decodeImmediate16T1 :: ThumbStreamState Word32
decodeImmediate16T1 = do
    imm8 <- instructionBits 0 8
    imm3 <- (`shiftL` 8) <$> instructionBits 12 3
    i <- (`shiftL` 11) <$> instructionBits 26 1
    imm4 <- (`shiftL` 12) <$> instructionBits 16 4
    return $ imm8 + imm3 + i + imm4

decodeImmediateBranchT4 :: ThumbStreamState Word32
decodeImmediateBranchT4 = do
    s <- instructionFlag 26
    i1 <-(not . (`bxor` s)) <$> instructionFlag 13
    i2 <-(not . (`bxor` s)) <$> instructionFlag 11
    

decodeImmediate12T1 :: ThumbStreamState Word32
decodeImmediate12T1 = do
    imm12first <- instructionArrayBits [26,14,13,12]
    case imm12first of
        [0,0,0,0] -> instructionBits 0 8
        [0,0,0,1] -> do 
            imm8 <- instructionBits 0 8
            return $ imm8 + (imm8*(2^15))
        [0,0,1,0] -> do
            imm8 <- instructionBits 0 8
            return $ (imm8*(2^7)) + (imm8 * (2^23))
        [0,0,1,1] -> do
            imm8 <- instructionBits 0 8
            return $ (imm8*(2^7)) + (imm8 * (2^23)) + imm8 + (imm8*(2^15))
        otherwise -> do
            a <- instructionBits 7 1
            i <- instructionBits 26 1
            imm3 <- instructionBits 12 3
            imm7 <- instructionBits 0 7
            return $ (imm7 + 0x100) `rotateR` (fromIntegral (a + (imm3*2) + (i*16)))

decodeImmediate5T2 :: ThumbStreamState Word32
decodeImmediate5T2 = ((+) <$> ((`shiftL` 2) <$> instructionBits 12 3) <*> instructionBits 6 2)

parseStream :: ByteString -> [ArmInstr]
parseStream s = fst (runState (parseInstrStream 150) (ThumbStream s 0 0 False))

