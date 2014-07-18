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

data ArmRegister = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

data ArmInstr = ArmInstr {cond :: Cond, op :: ArmInstrOp} | NotParsed
data ArmInstrOp = DRI {dri :: DataRegisterInstr }
data ArmInstrClass = DataProcessing | LoadStore | Branch | Coprocessor

data Cond = CondEQ | CondNE | CondCS | CondCC | CondMI | CondPL | CondVS | CondVC | CondHI | CondLS | CondGE | CondLT | CondGT | CondLE | CondAL | Uncond

data DataRegisterInstr = DRInstr { opcode :: DataInstrOp, rn :: ArmRegister, rd :: ArmRegister, immOrShift :: Either Bin.Word32 ArmRegister, shiftType :: SRType, rm :: ArmRegister}
data DataInstrClass = And | Eor | Sub | Rsb | Add | Adc | Sbc | Rsc | Tst | Teq | Cmp | Cmn | Orr | Mov | Lsl | Lsr | Asr | Rrx | Ror | Bic | Mvn
    deriving (Show)
data DataInstrOp  = DataInstrOp DataInstrClass SystemLevel
data SRType = ASR | LSL | LSR | ROR
data SystemLevel = SystemInst | NormalInst

data ArmStream = ArmStream ByteString Bin.Word32

type ArmStreamState a = State ArmStream a

instance Show DataRegisterInstr where
    show DRInstr {opcode=(DataInstrOp cl sys)} = show cl

instance Show ArmInstr where
    show ArmInstr {op=(DRI {dri=inst})} = show inst
    show NotParsed = "Unknown"

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
wordToRegister 11 = R11 
wordToRegister 12 = R12 
wordToRegister 13 = R13 
wordToRegister 14 = R14 
wordToRegister 15 = R15 

wordToSRType :: Bin.Word32 -> SRType
wordToSRType 0 = LSL
wordToSRType 1 = LSR
wordToSRType 2 = ASR
wordToSRType 3 = ROR

wordToDataClass :: Bin.Word32 -> DataInstrClass
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
    return $ (currentInst `shiftR` off) .&. ((round $ 2 ** (fromIntegral count)) - 1)

parseRegister :: Int -> ArmStreamState ArmRegister
parseRegister off = do 
    wordToRegister <$> instructionBits off 4

parseCond :: ArmStreamState Cond
parseCond = do
    cond <- instructionBits 0 4
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
    case condition of
        Uncond -> return NotParsed
        otherwise -> do
            cl <- parseInstructionClass
            case cl of
                DataProcessing -> do 
                    ope <- parseDataProcessing
                    return ArmInstr {cond=condition,op=ope}
                LoadStore -> return NotParsed
                Branch ->   return NotParsed
                Coprocessor -> return NotParsed

parseDataInstructionClass :: ArmStreamState DataInstrClass
parseDataInstructionClass = wordToDataClass <$>  instructionBits 21 4

parseDataInstructionOp :: ArmStreamState DataInstrOp
parseDataInstructionOp = do
    cl <- parseDataInstructionClass
    sys <- instructionBits 20 1
    case sys of
        0 -> return $ DataInstrOp cl NormalInst
        1 -> return $ DataInstrOp cl SystemInst
    
parseDataProcessing :: ArmStreamState ArmInstrOp
parseDataProcessing = do
    op <- parseDataInstructionOp
    regn <- parseRegister 16
    regd <- parseRegister 12
    temp <- instructionBits 7 5
    stype <- wordToSRType <$> instructionBits 5 2
    shiftCond <- instructionBits 4 1
    shiftInfo <- case shiftCond of 
        0 -> return $ Left temp
        1 -> return $ Right $ wordToRegister $ (temp `shiftR` 1)
    regm <- parseRegister 0
    return DRI {dri=DRInstr {opcode=op, rn=regn, rd=regd, rm=regm, immOrShift=shiftInfo, shiftType=stype}}
    

parseInstrStream :: Int -> ArmStreamState [ArmInstr]
parseInstrStream 0 = return []
parseInstrStream n = do
    nextArmInstruction 
    i <- parseArmInstruction
    (i:) <$> parseInstrStream (n-1)

parseStream :: ByteString -> [ArmInstr]
parseStream s = fst (runState (parseInstrStream 10) (ArmStream s 0))
