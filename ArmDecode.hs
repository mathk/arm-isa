module ArmDecode (
    ArmInstr, parseStream,
) where

--import qualified Data.Binary.Bits.Get as Bin
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import Data.Binary.Bits
import Data.Bits 
import Data.ByteString
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy
import Text.Printf
import Data.Tuple.All

data ArmRegister = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
    deriving (Show)

data ArmInstr = 
          ArmInstr {code :: Bin.Word32, cond :: Cond, memonic :: InstrClass, isFlags :: Bool, args :: ArgumentsInstruction  } 
        | NotParsed
        | Undefined

data ArmInstrClass = DataProcessing | LoadStore | Branch | Coprocessor

data Cond = CondEQ | CondNE | CondCS | CondCC | CondMI | CondPL | CondVS | CondVC | CondHI | CondLS | CondGE | CondLT | CondGT | CondLE | CondAL | Uncond

data ArgumentsInstruction = 
        RegisterShiftedArgs 
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rs register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
    |   RegisterShiftShiftedArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
    |   RegisterShiftedTestArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rs register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
    |   RegisterShiftedMvnArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rs register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
    |   RegisterArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rd regsiter
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Bin.Word32  -- ^ Immediate value to shift
    |   RegisterMovArgs 
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
    |   RegisterMvnArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Bin.Word32  -- ^ Immediate value to shift
    |   ImmediateArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rd register
            Bin.Word32  -- ^ Immediate value
    |   ImmediateMovArgs
            ArmRegister -- ^ The rd register
            Bin.Word32  -- ^ Immediate value
    |   ImmediateTestArgs
            ArmRegister -- ^ The rn register
            Bin.Word32  -- ^ The immediate value
    |   RegisterTestArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Bin.Word32  -- ^ Immediate value to shift
    |   ShiftArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            Bin.Word32  -- ^ Immediate value to shift
    |   BranchArgs
            Bin.Word32  -- ^ Immediate value to branch
{-- This is not used for the time being
    |   ImmediatePcArgs
            ArmRegister -- ^ The rd register
            Bin.Word32  -- ^ The immediate value --}

newtype Shift = Shift (SRType,Bin.Word32)

data InstrClass = And | Eor | Sub | Rsb | Add | Adc | Sbc | Rsc | Tst | Teq | Cmp | Cmn | Orr | Mov | Movw | Lsl | Lsr | Asr | Rrx | Ror | Bic | Mvn | B | Bl | Blx
    deriving (Show)

data SRType = ASR | LSL | LSR | ROR

data ArmStream = ArmStream ByteString Bin.Word32

type ArmStreamState a = State ArmStream a

instance Show ArgumentsInstruction where
    show (RegisterArgs rn rd rm st n) = printf "%s,%s,%s %s" (show rd) (show rn) (show rm) (show $ Shift (st,n))
    show (RegisterShiftShiftedArgs rd rm rn) = printf "%s,%s,%s %s" (show rd) (show rn)
    show (RegisterMovArgs rd rm) = printf "%s,%s" (show rd) (show rm)
    show (RegisterShiftedArgs rn rd rs rm st) = printf "%s,%s,%s %s %s" (show rd) (show rn) (show rm) (show st) (show rs)
    show (RegisterShiftedTestArgs rn rs rm st) = printf "%s,%s %s %s" (show rn) (show rm) (show st) (show rs)
    show (RegisterShiftedMvnArgs rd rs rm st) = printf "%s,%s %s %s" (show rd) (show rm) (show st) (show rs)
    show (ImmediateArgs rn rd imm) = printf "%s,%s, #%d" (show rd) (show rn) imm
    show (ImmediateMovArgs rd imm) = printf "%s, #%d" (show rd) imm
    show (RegisterTestArgs rn rm st n) = printf "%s, %s %s" (show rn) (show rm) (show $ Shift (st,n))
    show (RegisterMvnArgs rd rm st n) = printf "%s, %s %s" (show rd) (show rm) (show $ Shift (st,n))
    show (ShiftArgs rd rm n) = printf "%s, %s  #%d" (show rd) (show rm) n
    show (BranchArgs imm) = printf "<PC+%x>" imm

instance Show Shift where
    show (Shift (_,0)) = ""
    show (Shift (st,n)) = printf "%s #%d" (show st) n

instance Show SRType where
    show ASR = "asr"
    show LSL = "lsl"
    show LSR = "lsr"
    show ROR = "ror"

instance Show ArmInstr where
    show ArmInstr {code=co, cond=c,memonic=m, isFlags=flgs, args=arguments} = printf "%08X %s%s %s" co (show m) (show c) (show arguments)
    show NotParsed = "Unknown"
    show Undefined = "Undefined"

instance Show Cond where
    show CondEQ = ".eq"
    show CondNE = ".ne"
    show CondCS = ".cs"
    show CondCC = ".cc"
    show CondMI = ".mi"
    show CondPL = ".pl"
    show CondVS = ".vs"
    show CondVC = ".vc"
    show CondHI = ".hi"
    show CondLS = ".ls"
    show CondGE = ".ge"
    show CondLT = ".lt"
    show CondGT = ".gt"
    show CondLE = ".le"
    show CondAL = ""
    show Uncond = ""

wordToRegister :: Bin.Word32 -> ArmRegister
wordToRegister 0 = R0 
wordToRegister 1 = R1 
wordToRegister 2 = R2 
wordToRegister 3 = R3 
wordToRegister 4 = R4 
wordToRegister 5 = R5 
wordToRegister 6 = R6 
wordToRegister 7 = R7 
wordToRegister 8 = R8 
wordToRegister 9 = R9 
wordToRegister 10 = R10 
wordToRegister 11 = FP 
wordToRegister 12 = R12 
wordToRegister 13 = SP 
wordToRegister 14 = LR 
wordToRegister 15 = PC 

wordToSRType :: Bin.Word32 -> SRType
wordToSRType 0 = LSL
wordToSRType 1 = LSR
wordToSRType 2 = ASR
wordToSRType 3 = ROR

wordToDataClass :: Bin.Word32 -> InstrClass
wordToDataClass 0 = And
wordToDataClass 1 = Eor
wordToDataClass 2 = Sub
wordToDataClass 3 = Rsb
wordToDataClass 4 = Add
wordToDataClass 5 = Adc
wordToDataClass 6 = Sbc
wordToDataClass 7 = Rsc
wordToDataClass 8 = Tst
wordToDataClass 9 = Teq
wordToDataClass 10 = Cmp
wordToDataClass 11 = Cmn
wordToDataClass 12 = Orr
wordToDataClass 13 = Mov
wordToDataClass 14 = Bic
wordToDataClass 15 = Mvn

nextArmInstruction :: ArmStreamState ()
nextArmInstruction = do
    (ArmStream s _) <- get
    case Bin.pushChunk (Bin.runGetIncremental Bin.getWord32le) s of
        Bin.Done resultS off word -> do
            put $  ArmStream resultS word

instructionBits :: Int -> Int -> ArmStreamState Bin.Word32
instructionBits off count = do
    (ArmStream _ currentInst) <- get
    return $ (currentInst `shiftR` off) .&. ((2 ^ count) - 1)

instructionSignedExtendBits :: Int -> Int -> ArmStreamState Bin.Word32
instructionSignedExtendBits off count = do
    sig <- instructionBits (count - 1) 1
    case sig of 
        0 -> instructionBits off count
        1 -> (.|. complement ((2^count) - 1) ) <$> instructionBits off count

instructionArrayBits :: [Int] -> ArmStreamState [Bin.Word32]
instructionArrayBits = sequence . (fmap (`instructionBits` 1))

parseRegister :: Int -> ArmStreamState ArmRegister
parseRegister off = do 
    wordToRegister <$> instructionBits off 4

parseCond :: ArmStreamState Cond
parseCond = do
    cond <- instructionBits 28 4
    case cond of
        0 -> return CondEQ
        1 -> return CondNE
        2 -> return CondCS
        3 -> return CondCC
        4 -> return CondMI
        5 -> return CondPL
        6 -> return CondVS
        7 -> return CondVC
        8 -> return CondHI
        9 -> return CondLS
        10 -> return CondGE
        11 -> return CondLT
        12 -> return CondGT
        13 -> return CondLE
        14 -> return CondAL
        15 -> return Uncond

parseInstructionClass :: ArmStreamState ArmInstrClass
parseInstructionClass = do
    cl <- instructionBits 26 2
    case cl of
        0 -> return DataProcessing
        1 -> return LoadStore
        2 -> return Branch
        3 -> return Coprocessor

parseArmInstruction :: ArmStreamState ArmInstr
parseArmInstruction = do
    condition <- parseCond
    inst <- instructionBits 0 32
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
        Just (cl,flags,arguments) -> return $ ArmInstr {code=inst, cond=condition, memonic=cl, isFlags=flags, args=arguments}
        Nothing -> return Undefined

-- | Branch, branch with link and block data transfert
parseBranchAndBlockTransfer :: ArmStreamState (Maybe (InstrClass,Bool,ArgumentsInstruction))
parseBranchAndBlockTransfer = do
    bitsField <- instructionArrayBits [25,24,23,22,21,20,15,19,18,17,16] 
    case bitsField of 
        [1,0,_,_,_,_, _, _,_,_,_] -> parseBranch
        [1,1,_,_,_,_, _, _,_,_,_] -> parseBranchLink
        otherwise -> return Nothing

-- | Instruction with condition set to unconditional excluding SIMD
parseUnconditional :: ArmStreamState (Maybe (InstrClass, Bool,ArgumentsInstruction))
parseUnconditional = do
    bitsField <- instructionArrayBits [27,26,25,24,23,22,21,20,19,18,17,16,4]
    case bitsField of 
        [1,0,1,_,_,_,_,_, _,_,_,_, _] -> parseUncondBranch
        otherwise -> return Nothing

parseBranch :: ArmStreamState (Maybe (InstrClass,Bool,ArgumentsInstruction))
parseBranch = do
    args <- parseBranchArgument
    return $ Just (B,False,args)

parseUncondBranch :: ArmStreamState (Maybe (InstrClass,Bool,ArgumentsInstruction))
parseUncondBranch = do
    args <- parseUncondBranchArgument
    return $ Just (Blx,False,args)

parseBranchLink :: ArmStreamState (Maybe (InstrClass,Bool,ArgumentsInstruction))
parseBranchLink = do
    args <- parseBranchArgument
    return $ Just (Bl,False,args) 

parseDataProcessing :: ArmStreamState (Maybe (InstrClass, Bool, ArgumentsInstruction))
parseDataProcessing = do
    bitsField <- instructionArrayBits [25,24,23,22,21,20,7,6,5,4]
    case bitsField of
        [0,_,_,_,_,_,     _,_,_,0] -> parseDataProcessingRegister
        [0,_,_,_,_,op120, 0,_,_,1] -> parseDataProcessingRegisterShift
        [1,1,0,0,0,0,     _,_,_,_] -> do
            args <- parseImmediateMov16Argument
            return $ Just (Movw, False, args)
        [1,_,_,_,_,_,    _,_,_,_] -> parseDataProcessingImmediate
        otherwise -> return Nothing

parseDataProcessingRegisterShift :: ArmStreamState (Maybe (InstrClass,Bool,ArgumentsInstruction))
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

parseDataProcessingImmediate :: ArmStreamState (Maybe (InstrClass,Bool,ArgumentsInstruction))
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

parseDataProcessingRegister :: ArmStreamState (Maybe (InstrClass, Bool, ArgumentsInstruction))
parseDataProcessingRegister = do
    imm <- instructionBits 7 5
    isFlags  <- (`testBit` 0) <$> instructionBits 20 1
    argReg <- parseRegisterArgument
    argTest <- parseRegisterTestArgument
    argMov <- parseRegisterMovArgs
    argShift <- parseShiftArgument
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
        [1,1,0,1,_, 0,0, _] -> return $ Just (Lsl,isFlags,argShift)
        [1,1,0,1,_, 0,1, _] -> return $ Just (Lsr,isFlags,argShift)
        [1,1,0,1,_, 1,0, _] -> return $ Just (Asr,isFlags,argShift)
        [1,1,0,1,_, 1,1, 0] -> do
            case argMov of
                Nothing -> return Nothing
                Just mov -> return $ Just (Rrx,isFlags,mov)
        [1,1,0,1,_, 1,1, _] -> return $ Just (Ror,isFlags,argShift)
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

parseRegisterMovArgs :: ArmStreamState (Maybe ArgumentsInstruction)
parseRegisterMovArgs = do
    regd <- parseRegister 12
    regm <- parseRegister 0
    assert <- instructionBits 4 8
    case assert of
        0 -> return $ Just $ RegisterMovArgs regd regm
        _ -> return Nothing
    
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

parseBranchArgument :: ArmStreamState ArgumentsInstruction
parseBranchArgument = (BranchArgs . (`shiftL` 2)) <$> instructionSignedExtendBits 0 24

parseUncondBranchArgument :: ArmStreamState ArgumentsInstruction
parseUncondBranchArgument = do
    h <- instructionBits 24 1
    (BranchArgs . (`shift` 1) . (+h) . (`shiftL` 1)) <$> instructionSignedExtendBits 0 24

parseShiftArgument :: ArmStreamState ArgumentsInstruction
parseShiftArgument = do
    regd <- parseRegister 12
    regm <- parseRegister 0
    imm     <- instructionBits 7 5
    return $ ShiftArgs regd regm imm

parseInstrStream :: Int -> ArmStreamState [ArmInstr]
parseInstrStream 0 = return []
parseInstrStream n = do
    nextArmInstruction 
    i <- parseArmInstruction
    (i:) <$> parseInstrStream (n-1)

parseStream :: ByteString -> [ArmInstr]
parseStream s = fst (runState (parseInstrStream 30) (ArmStream s 0))
