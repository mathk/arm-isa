{-# LANGUAGE FlexibleContexts #-}

module Arm.Core (
    ArmBlock,
    parseThumbStream, parseArmStream,
    parseThumbBlock, parseArmBlock,
    instructionsBlock, nextBlocks,
    nextBlocksFromInstruction,
)
where

import Arm.Internal.Type
import qualified Arm.ThumbDecode as Thumb
import qualified Arm.ArmDecode as Arm
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.List
import Data.Int (Int64)

-- | Parse a stream of instruction
parseInstrStream :: InstructionStreamState m =>  Int -> m [ArmInstr]
parseInstrStream 0 = return []
parseInstrStream n = do
    nextInstruction 
    i <- parseInstruction
    (i:) <$> parseInstrStream (n-1)

parseBlockStreamWhile :: InstructionStreamState m => (ArmInstr -> Bool) -> m ArmBlock
parseBlockStreamWhile test = do
    nextInstruction
    i <- parseInstruction
    state <- decodingState
    if test i 
    then ArmBlock <$> pure (return i) <*> (pure $ nextBlocksFromInstruction state i) <*> decodingState
    else do 
        block <- parseBlockStreamWhile test 
        ArmBlock (i:instructionsBlock block) (nextBlocks block) <$> decodingState

isBlockEnding :: ArmInstr -> Bool
isBlockEnding ArmInstr {memonic=B} = True
isBlockEnding ArmInstr {memonic=Bx} = True
isBlockEnding ArmInstr {memonic=Bxj} = True
isBlockEnding ArmInstr {memonic=Bl} = True
isBlockEnding ArmInstr {memonic=Blx} = True
isBlockEnding ArmInstr {memonic=Pop, args=(RegisterListArgs lst)} = PC `elem` lst
isBlockEnding ArmInstr {memonic=Ldm, args=(LoadAndStoreRegisterListArgs _ _ lst)} = PC `elem` lst
isBlockEnding ArmInstr {args=(RegisterShiftedArgs _ PC _ _ _)}= True
isBlockEnding ArmInstr {args=(RegisterShiftShiftedArgs PC _ _)} = True
isBlockEnding ArmInstr {args=(RegisterShiftedMvnArgs PC _ _ _)} = True
isBlockEnding ArmInstr {args=(RegisterArgs _ PC _ _ _)} = True
isBlockEnding ArmInstr {args=(RegisterToRegisterArgs PC _)} = True
isBlockEnding _ = False

-- | Retrive the next block offset from an instruction
nextBlocksFromInstruction :: InstructionState -> ArmInstr -> [Int64]
nextBlocksFromInstruction state ArmInstr {sectionOffset=off, memonic=Blx, args=(BranchArgs imm)} = [nextPc off state, fromIntegral imm + nextPc off state]
nextBlocksFromInstruction _ _ = []


nextPc :: Int64 -> InstructionState -> Int64
nextPc off ArmState = 8 + off
nextPc off ThumbState = 4 + off

parseThumbStream :: B.ByteString -> Int -> [ArmInstr]
parseThumbStream s n = fst (runState (parseInstrStream n) (Thumb.initialState s))

parseArmStream :: B.ByteString -> Int -> [ArmInstr]
parseArmStream s n = fst (runState (parseInstrStream n) (Arm.initialState s))

parseBlock :: InstructionStreamState (State a) => (B.ByteString -> a) -> B.ByteString -> ArmBlock
parseBlock init s = fst (runState (parseBlockStreamWhile isBlockEnding) (init s)) 

parseThumbBlock :: Int64 -> B.ByteString -> ArmBlock
parseThumbBlock n = (parseBlock Thumb.initialState) . (B.drop $ fromIntegral n)

parseArmBlock :: Int64 -> B.ByteString -> ArmBlock
parseArmBlock n = (parseBlock Arm.initialState) . (B.drop $ fromIntegral n)

instructionsBlock :: ArmBlock -> [ArmInstr]
instructionsBlock (ArmBlock lst _ _) = lst

nextBlocks :: ArmBlock -> [Int64]
nextBlocks (ArmBlock _ lst _) = lst
