{-# LANGUAGE FlexibleInstances #-}

module ArmDecode (
    parseStream,
) where

import Internal.Type
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import Data.Binary.Bits
import Data.Bits 
import Data.ByteString
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy
import Data.Tuple.All
import Data.Int (Int64)

data ArmInstrClass = DataProcessing | LoadStore | Branch | Coprocessor

data ArmStream = ArmStream Int64 ByteString Bin.Word32

type ArmStreamState a = State ArmStream a

type ArmInstructionPart = Maybe (InstrClass,Bool,ArgumentsInstruction)

instance InstructionStreamState (State ArmStream) where
    instructionBits off count = do
        (ArmStream _ _ currentInst) <- get
        return $ (currentInst `shiftR` off) .&. ((2 ^ count) - 1)
    nextInstruction = nextArmInstruction
    parseInstruction = parseArmInstruction
    instructionOpcode = instructionBits 0 32
    instructionOffset = getOffset

-- | Get the current instruction offset from the begining 
-- of the current section.
getOffset :: ArmStreamState Int64
getOffset = do
    (ArmStream nextOffset _ _) <- get
    return $ nextOffset - 4

nextArmInstruction :: ArmStreamState ()
nextArmInstruction = do
    (ArmStream nextOffset s _) <- get
    case Bin.pushChunk (Bin.runGetIncremental Bin.getWord32le) s of
        Bin.Done resultS off word -> do
            put $  ArmStream (nextOffset + 4) resultS word

justArgumentPart :: InstrClass -> Bool -> ArgumentsInstruction -> ArmInstructionPart
justArgumentPart cl flag instr = Just (cl,flag,instr)

parseCond :: ArmStreamState Cond
parseCond = parseCondAt 28

parseInstructionClass :: ArmStreamState ArmInstrClass
parseInstructionClass = do
    cl <- instructionBits 26 2
    case cl of
        0 -> return DataProcessing
        1 -> return LoadStore
        2 -> return Branch
        3 -> return Coprocessor

-- | This is the entry point of the ARM instruction set decoder
parseArmInstruction :: ArmStreamState ArmInstr
parseArmInstruction = do
    condition <- parseCond
    part <- case condition of
        Uncond -> parseUnconditional
        otherwise -> do
            cl <- parseInstructionClass
            case cl of
                DataProcessing -> parseDataProcessing
                LoadStore -> return $ Nothing
                Branch -> parseBranchAndBlockTransfer
                Coprocessor -> return $ Nothing
    case part of
        Just (cl,flags,arguments) -> ArmInstr <$> getOffset <*> instructionBits 0 32 <*> parseCond <*> return cl <*> return flags <*> return  arguments
        Nothing -> undefinedInstruction
 
-- | Branch, branch with link and block data transfert
parseBranchAndBlockTransfer :: ArmStreamState ArmInstructionPart
parseBranchAndBlockTransfer = do
    bitsField <- instructionArrayBits [25,24,23,22,21,20,15,19,18,17,16] 
    case bitsField of 
        [1,0,_,_,_,_, _, _,_,_,_] -> parseBranch
        [1,1,_,_,_,_, _, _,_,_,_] -> parseBranchLink
        otherwise -> return Nothing

-- | Instruction with condition set to unconditional excluding SIMD
parseUnconditional :: ArmStreamState ArmInstructionPart
parseUnconditional = do
    bitsField <- instructionArrayBits [27,26,25,24,23,22,21,20,19,18,17,16,4]
    case bitsField of 
        [1,0,1,_,_,_,_,_, _,_,_,_, _] -> parseUncondBranch
        otherwise -> return Nothing

parseBranch :: ArmStreamState ArmInstructionPart
parseBranch = do
    args <- parseBranchArgument
    return $ Just (B,False,args)

parseUncondBranch :: ArmStreamState ArmInstructionPart
parseUncondBranch = do
    args <- parseUncondBranchArgument
    return $ Just (Blx,False,args)

parseBranchLink :: ArmStreamState ArmInstructionPart
parseBranchLink = do
    args <- parseBranchArgument
    return $ Just (Bl,False,args)  

parseDataProcessing :: ArmStreamState ArmInstructionPart
parseDataProcessing = do
    bitsField <- instructionArrayBits [25,24,23,22,21,20,7,6,5,4]
    case bitsField of
        [0,1,0,_,_,0, 0,_,_,_] -> parseMiscellaneous
        [0,1,0,_,_,0, 1,_,_,0] -> parseHalfwordMultiplyAcc
        [0,1,0,_,_,0, _,_,_,_] -> return Nothing
        [0,_,_,_,_,_, _,_,_,0] -> parseDataProcessingRegister
        [0,_,_,_,_,_, 0,_,_,1] -> parseDataProcessingRegisterShift
        [1,1,0,0,0,0, _,_,_,_] -> justArgumentPart Movw False <$> parseImmediateMov16Argument
        [1,_,_,_,_,_, _,_,_,_] -> parseDataProcessingImmediate
        otherwise -> return Nothing

parseHalfwordMultiplyAcc :: ArmStreamState ArmInstructionPart
parseHalfwordMultiplyAcc = do 
    bitsField <- instructionArrayBits [22,21,5]
    case bitsField of 
        [0,0,_] -> justArgumentPart Smla    False <$> parseSignMultiplyHalfAccArguement
        [0,1,0] -> justArgumentPart Smlaw   False <$> parseSignMultiplyWordHalfAccArgument
        [0,1,1] -> justArgumentPart Smulw   False <$> parseSignMultiplyWordHalfArgument
        [1,0,_] -> justArgumentPart Smlal   False <$> parseSignMultiplyLongAccArgument
        [1,1,_] -> justArgumentPart Smul    False <$> parseSignMultiplyHalfArgument

parseMiscellaneous :: ArmStreamState ArmInstructionPart
parseMiscellaneous = do
    bitsField <- instructionArrayBits [6,5,4,9,22,21,19,18,17,16]
    argBx <- parseBranchExchangeArgument
    argReg <- parseRegisterToRegisterArgument 
    case bitsField of 
        [0,0,0, _, _,_, _,_,_,_] -> return $ Just (Msr,False,NullArgs)
        [0,0,1, _, 0,1, _,_,_,_] -> return $ Just (Bx,False,argBx)
        [0,0,1, _, 1,1, _,_,_,_] -> return $ Just (Clz,False,argReg)
        [0,1,0, _, 0,1, _,_,_,_] -> return $ Just (Bxj,False,argBx)
        [0,1,1, _, 0,1, _,_,_,_] -> return $ Just (Blx,False,argBx)
        [1,1,0, _, 1,1, _,_,_,_] -> return $ Just (Eret,False,NoArgs)
        [1,1,1, _, 0,1, _,_,_,_] -> return $ Just (Bkpt,False,NullArgs)
        [1,1,1, _, 1,0, _,_,_,_] -> return $ Just (Hvc,False,NullArgs)
        [1,1,1, _, 1,1, _,_,_,_] -> return $ Just (Smc,False,NullArgs)
        otherwise -> return Nothing

parseDataProcessingRegisterShift :: ArmStreamState ArmInstructionPart
parseDataProcessingRegisterShift = do
    bitsField <- instructionArrayBits [24,23,22,21,20,6,5]
    isFlags     <- (`testBit` 0) <$> instructionBits 20 1
    argRegShift <- parseRegisterShiftArgument
    argRegShiftTest <- parseRegisterShiftTestArgument
    argShiftShift <- parseRegisterShiftShiftArgument
    argMvn <- parseRegisterShiftMvnArgument
    case bitsField of 
        [0,0,0,0,_, _,_] -> return $ Just (And,isFlags,argRegShift)
        [0,0,0,1,_, _,_] -> return $ Just (Eor,isFlags,argRegShift)
        [0,0,1,0,_, _,_] -> return $ Just (Sub,isFlags,argRegShift)
        [0,0,1,1,_, _,_] -> return $ Just (Rsb,isFlags,argRegShift)
        [0,1,0,0,_, _,_] -> return $ Just (Add,isFlags,argRegShift)
        [0,1,0,1,_, _,_] -> return $ Just (Adc,isFlags,argRegShift)
        [0,1,1,0,_, _,_] -> return $ Just (Sbc,isFlags,argRegShift)
        [0,1,1,1,_, _,_] -> return $ Just (Rsc,isFlags,argRegShift)
        [1,0,0,0,1, _,_] -> return $ Just (Tst,True,argRegShiftTest)
        [1,0,0,1,1, _,_] -> return $ Just (Teq,True,argRegShiftTest)
        [1,0,1,0,1, _,_] -> return $ Just (Cmp,True,argRegShiftTest)
        [1,0,1,1,1, _,_] -> return $ Just (Cmn,True,argRegShiftTest)
        [1,1,0,0,_, _,_] -> return $ Just (Orr,isFlags,argShiftShift)
        [1,1,0,1,_, 0,0] -> return $ Just (Lsl,isFlags,argShiftShift)
        [1,1,0,1,_, 0,1] -> return $ Just (Lsr,isFlags,argShiftShift)
        [1,1,0,1,_, 1,0] -> return $ Just (Asr,isFlags,argShiftShift)
        [1,1,0,1,_, 1,1] -> return $ Just (Ror,isFlags,argShiftShift)
        [1,1,1,0,_, _,_] -> return $ Just (Bic,isFlags,argRegShift)
        [1,1,0,1,_, _,_] -> return $ Just (Mvn,isFlags,argMvn)

parseDataProcessingImmediate :: ArmStreamState ArmInstructionPart
parseDataProcessingImmediate = do
    bitsField   <- instructionArrayBits [24,23,22,21,20,19,18,17,16]
    isFlags     <- (`testBit` 0) <$> instructionBits 20 1
    argImmMvn   <- parseImmediateMvnArgument
    argImmMov   <- parseImmediateMovArgument
    argImm      <- parseImmediateArgument
    argImmTest  <- parseImmediateTestArgument
    case bitsField of
        [0,0,0,0,_, _,_,_,_]  -> return $ Just (And,isFlags,argImm)
        [0,0,0,1,_, _,_,_,_]  -> return $ Just (Eor,isFlags,argImm)
        --[0,0,1,0,0, 1,1,1,1]  -> (Sub,isFlags,argImm) Not used
        [0,0,1,0,1, 1,1,1,1]  -> return Nothing
        [0,0,1,0,_, _,_,_,_]  -> return $ Just (Sub,isFlags,argImm)
        [0,0,1,1,_, _,_,_,_]  -> return $ Just (Rsb,isFlags,argImm)
        --[0,1,0,0,0, 1,1,1,1]  -> (Add,isFlags,argImm) Not used
        [0,1,0,0,1, 1,1,1,1]  -> return Nothing
        [0,1,0,0,_, _,_,_,_]  -> return $ Just (Add,isFlags,argImm)
        [0,1,0,1,_, _,_,_,_]  -> return $ Just (Adc,isFlags,argImm)
        [0,1,1,0,_, _,_,_,_]  -> return $ Just (Sbc,isFlags,argImm)
        [0,1,1,1,_, _,_,_,_]  -> return $ Just (Rsc,isFlags,argImm)
        [1,0,0,0,1, _,_,_,_]  -> return $ Just (Tst,True,argImmTest)
        [1,0,0,1,1, _,_,_,_]  -> return $ Just (Teq,True,argImmTest)
        [1,0,1,0,1, _,_,_,_]  -> return $ Just (Cmp,True,argImmTest)
        [1,0,1,1,1, _,_,_,_]  -> return $ Just (Cmn,True,argImmTest)
        [1,1,0,0,_, _,_,_,_]  -> return $ Just (Orr,isFlags,argImm)
        [1,1,0,1,_, _,_,_,_]  -> return $ Just (Movw,isFlags,argImmMov)
        [1,1,1,0,_, _,_,_,_]  -> return $ Just (Bic,isFlags,argImm)
        [1,1,1,1,_, _,_,_,_]  -> return $ Just (Mvn,isFlags,argImmMvn)
        otherwise -> return Nothing

parseDataProcessingRegister :: ArmStreamState ArmInstructionPart
parseDataProcessingRegister = do
    imm <- instructionBits 7 5
    isFlags  <- (`testBit` 0) <$> instructionBits 20 1
    argReg <- parseRegisterArgument
    argTest <- parseRegisterTestArgument
    argMov <- parseRegisterMovArgument
    argMvn <- parseRegisterShiftMvnArgument
    bitsField <- instructionArrayBits [24,23,22,21,20,6,5]
    case bitsField ++ [imm] of 
        [0,0,0,0,_, _,_, _] -> return $ Just (And,isFlags,argReg)
        [0,0,0,1,_, _,_, _] -> return $ Just (Eor,isFlags,argReg)
        [0,0,1,0,_, _,_, _] -> return $ Just (Sub,isFlags,argReg)
        [0,0,1,1,_, _,_, _] -> return $ Just (Rsb,isFlags,argReg)
        [0,1,0,0,_, _,_, _] -> return $ Just (Add,isFlags,argReg)
        [0,1,0,1,_, _,_, _] -> return $ Just (Adc,isFlags,argReg)
        [0,1,1,0,_, _,_, _] -> return $ Just (Sbc,isFlags,argReg)
        [0,1,1,1,_, _,_, _] -> return $ Just (Rsc,isFlags,argReg)
        [1,0,0,0,1, _,_, _] -> return $ Just (Tst,True,argTest)
        [1,0,0,1,1, _,_, _] -> return $ Just (Teq,True,argTest)
        [1,0,1,0,1, _,_, _] -> return $ Just (Cmp,True,argTest)
        [1,0,1,1,1, _,_, _] -> return $ Just (Cmn,True,argTest)
        [1,1,0,0,_, _,_, _] -> return $ Just (Orr,isFlags,argReg)
        [1,1,0,1,_, 0,0, 0] -> do 
            case argMov of
                Nothing -> return Nothing
                Just mov -> return $ Just (Mov,isFlags,mov)
        [1,1,0,1,_, 0,0, _] -> justArgumentPart Lsl isFlags <$> parseShiftArgument LSL
        [1,1,0,1,_, 0,1, _] -> justArgumentPart Lsr isFlags <$> parseShiftArgument LSR
        [1,1,0,1,_, 1,0, _] -> justArgumentPart Asr isFlags <$> parseShiftArgument ASR
        -- TODO: Can we use the parseShiftArgument instead?
        [1,1,0,1,_, 1,1, 0] -> do
            case argMov of
                Nothing -> return Nothing
                Just mov -> return $ Just (Rrx,isFlags,mov)
        [1,1,0,1,_, 1,1, _] -> justArgumentPart Ror isFlags <$> parseShiftArgument ROR
        [1,1,1,0,_, _,_, _] -> return $ Just (Bic,isFlags,argReg)
        [1,1,1,1,_, _,_, _] -> return $ Just (Mvn,isFlags,argMvn)
        otherwise -> return Nothing
        
parseRegisterShiftArgument :: ArmStreamState ArgumentsInstruction
parseRegisterShiftArgument = do
    regn    <- parseRegister 16
    regd    <- parseRegister 12
    stype   <- wordToSRType <$> instructionBits 5 2
    regm    <- parseRegister 0
    regs    <- parseRegister 8
    return $ RegisterShiftedArgs regn regd regs regm stype

parseRegisterArgument :: ArmStreamState ArgumentsInstruction
parseRegisterArgument = do    
    regn    <- parseRegister 16
    regd    <- parseRegister 12
    stype   <- wordToSRType <$> instructionBits 5 2
    regm    <- parseRegister 0
    imm     <- instructionBits 7 5
    return $ RegisterArgs regn regd regm stype imm

parseRegisterMovArgument :: ArmStreamState (Maybe ArgumentsInstruction)
parseRegisterMovArgument = do
    regd <- parseRegister 12
    regm <- parseRegister 0
    assert <- instructionBits 4 8
    case assert of
        0 -> return $ Just $ RegisterToRegisterArgs regd regm
        _ -> return Nothing

parseRegisterToRegisterArgument :: ArmStreamState ArgumentsInstruction
parseRegisterToRegisterArgument = do
    regd <- parseRegister 12
    regm <- parseRegister 0
    return $ RegisterToRegisterArgs regd regm
    
    
parseImmediateArgument :: ArmStreamState ArgumentsInstruction
parseImmediateArgument = do 
    regn <- parseRegister 16
    regd <- parseRegister 12
    imm <- instructionBits 0 12
    return $ ImmediateArgs regn regd imm

parseImmediateTestArgument :: ArmStreamState ArgumentsInstruction
parseImmediateTestArgument = do 
    regn <- parseRegister 16
    imm <- instructionBits 0 12
    return $ ImmediateTestArgs regn imm

parseImmediateMov16Argument :: ArmStreamState ArgumentsInstruction
parseImmediateMov16Argument = do
    immLow <- instructionBits 0 12
    immHigh <- instructionBits 16 4
    regd <- parseRegister 12
    return $ ImmediateMovArgs regd (immLow + (immHigh `shiftL` 12))

parseImmediateMovArgument :: ArmStreamState ArgumentsInstruction
parseImmediateMovArgument = do
    imm <- instructionBits 0 12
    regd <- parseRegister 12
    return $ ImmediateMovArgs regd imm

parseImmediateMvnArgument :: ArmStreamState ArgumentsInstruction
parseImmediateMvnArgument = do
    imm <- instructionBits 0 8
    rot <- (fromIntegral . (*2)) <$> instructionBits 8 4
    regd <- parseRegister 12
    return $ ImmediateMovArgs regd (imm `rotateR` rot)

parseRegisterTestArgument :: ArmStreamState ArgumentsInstruction
parseRegisterTestArgument = do
    regn <- parseRegister 16
    regm <- parseRegister 0
    stype <- wordToSRType <$> instructionBits 5 2
    imm <- instructionBits 7 5
    return $ RegisterTestArgs regn regm stype imm

parseRegisterMvnArgument :: ArmStreamState ArgumentsInstruction
parseRegisterMvnArgument = do
    regd <- parseRegister 12
    regm <- parseRegister 0
    stype <- wordToSRType <$> instructionBits 5 2
    imm <- instructionBits 7 5
    return $ RegisterMvnArgs regd regm stype imm

parseRegisterShiftTestArgument :: ArmStreamState ArgumentsInstruction
parseRegisterShiftTestArgument = do
    regn <- parseRegister 16
    regs <- parseRegister 8
    regm <- parseRegister 0
    stype <- wordToSRType <$> instructionBits 5 2
    return $ RegisterShiftedTestArgs regn regs regm stype

parseRegisterShiftMvnArgument :: ArmStreamState ArgumentsInstruction
parseRegisterShiftMvnArgument = do
    regd <- parseRegister 12
    regs <- parseRegister 8
    regm <- parseRegister 0
    stype <- wordToSRType <$> instructionBits 5 2
    return $ RegisterShiftedMvnArgs regd regs regm stype

parseRegisterShiftShiftArgument :: ArmStreamState ArgumentsInstruction
parseRegisterShiftShiftArgument = do
    regd <- parseRegister 12
    regm <- parseRegister 8
    regn <- parseRegister 0
    return $ RegisterShiftShiftedArgs regd regm regn

parseSignMultiplyHalfAccArguement :: ArmStreamState ArgumentsInstruction
parseSignMultiplyHalfAccArguement =
    MultiplyAccArgs <$> 
        parseRegister 16 <*> 
        parseRegister 12 <*>
        parseRegister 8 <*>
        parseRegister 0 <*>
        instructionFlag 6 <*>
        instructionFlag 5

parseSignMultiplyWordHalfAccArgument :: ArmStreamState ArgumentsInstruction
parseSignMultiplyWordHalfAccArgument =
    MultiplyAccWordArgs <$>
        parseRegister 16 <*> 
        parseRegister 12 <*>
        parseRegister 8 <*>
        parseRegister 0 <*>
        instructionFlag 6

parseSignMultiplyWordHalfArgument :: ArmStreamState ArgumentsInstruction
parseSignMultiplyWordHalfArgument =
    MultiplyHalfArgs <$>
        parseRegister 16 <*> 
        parseRegister 8 <*>
        parseRegister 0 <*>
        instructionFlag 6

parseSignMultiplyLongAccArgument :: ArmStreamState ArgumentsInstruction
parseSignMultiplyLongAccArgument = do
    MultiplyAccLongArgs <$>
        parseRegister 16 <*> 
        parseRegister 12 <*>
        parseRegister 8 <*>
        parseRegister 0 <*>
        instructionFlag 6 <*>
        instructionFlag 5

parseSignMultiplyHalfArgument :: ArmStreamState ArgumentsInstruction
parseSignMultiplyHalfArgument = 
    MultiplyArgs <$>
        parseRegister 16 <*> 
        parseRegister 8 <*>
        parseRegister 0 <*>
        instructionFlag 6 <*>
        instructionFlag 5

parseBranchArgument :: ArmStreamState ArgumentsInstruction
parseBranchArgument = (BranchArgs . (`shiftL` 2)) <$> instructionSignedExtendBits 0 24

parseUncondBranchArgument :: ArmStreamState ArgumentsInstruction
parseUncondBranchArgument = do
    h <- instructionBits 24 1
    (BranchArgs . (`shift` 1) . (+h) . (`shiftL` 1)) <$> instructionSignedExtendBits 0 24

parseBranchExchangeArgument :: ArmStreamState ArgumentsInstruction
parseBranchExchangeArgument = BranchExchangeArgs <$> parseRegister 0

parseShiftArgument :: SRType -> ArmStreamState ArgumentsInstruction
parseShiftArgument st = do
    regd <- parseRegister 12
    regm <- parseRegister 0
    Shift (st,imm)     <- decodeImmediateShift st <$> instructionBits 7 5
    return $ ShiftArgs regd regm imm

parseStream :: ByteString -> [ArmInstr]
parseStream s = fst (runState (parseInstrStream 50) (ArmStream 0 s 0))
