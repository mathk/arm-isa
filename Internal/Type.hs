module Internal.Type (
    wordToSRType, wordToRegister, instructionSignedExtendBits, instructionArrayBits, instructionFlag,
    InstructionStreamState(..),
    ArmRegister(..),
    ArmInstr(..),
    ArgumentsInstruction(..),
    Cond(..),
    InstrClass(..),
    SRType(..),
) where

import Data.Binary
import Text.Printf
import Control.Applicative
import Data.Bits

class (Functor m, Monad m) => InstructionStreamState m where
    instructionBits :: Int -> Int -> m Word32

data ArmRegister = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
    deriving (Show)

data ArmInstr = 
          ArmInstr {code :: Word32, cond :: Cond, memonic :: InstrClass, isFlags :: Bool, args :: ArgumentsInstruction  } 
        | NotParsed
        | Undefined

newtype Shift = Shift (SRType, Word32)

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
            Word32  -- ^ Immediate value to shift
    |   RegisterToRegisterArgs 
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
    |   RegisterMvnArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Word32  -- ^ Immediate value to shift
    |   ImmediateArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rd register
            Word32  -- ^ Immediate value
    |   ImmediateMovArgs
            ArmRegister -- ^ The rd register
            Word32  -- ^ Immediate value
    |   ImmediateTestArgs
            ArmRegister -- ^ The rn register
            Word32  -- ^ The immediate value
    |   RegisterTestArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Word32  -- ^ Immediate value to shift
    |   ShiftArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            Word32  -- ^ Immediate value to shift
    |   BranchArgs
            Word32  -- ^ Immediate value to branch
    |   BranchExchangeArgs
            ArmRegister -- ^ The rm register
    |   MultiplyAccArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The ra register
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
            Bool        -- ^ If we take the high bits of rm
            Bool        -- ^ If we take the high bits of rn
    |   MultiplyAccWordArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The ra register
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
            Bool        -- ^ If we take the high bits of rm
    |   MultiplyHalfArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
            Bool        -- ^ If we take the high bits of rm
    |   MultiplyAccLongArgs
            ArmRegister -- ^ The rdhi register
            ArmRegister -- ^ The rdlo register
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
            Bool        -- ^ If we take the high bits of rm
            Bool        -- ^ If we take the high bits of rn
    |   MultiplyArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
            Bool        -- ^ If we take the high bits of rm
            Bool        -- ^ If we take the high bits of rn
    |   NoArgs          -- ^ Instruction with no argument
    |   NullArgs        -- ^ For instruction that is not parsed

{-- This is not used for the time being
    |   ImmediatePcArgs
            ArmRegister -- ^ The rd register
            Word32  -- ^ The immediate value --}

data InstrClass = And | Eor | Sub | Rsb | Add | Adc | Sbc | Rsc | Tst | Teq | Clz | Cmp | Cmn | Orr | Mov | Movw | Lsl | Lsr | Asr | Rrx | Ror | Bic | Mvn | Msr | B | Bl | Blx | Bx | Bxj | Eret | Bkpt | Hvc | Smc | Smla | Smlaw | Smulw | Smlal | Smul
    deriving (Show)

data SRType = ASR | LSL | LSR | ROR

instance Show ArgumentsInstruction where
    show (RegisterArgs rn rd rm st n) = printf "%s,%s,%s %s" (show rd) (show rn) (show rm) (show $ Shift (st,n))
    show (RegisterShiftShiftedArgs rd rm rn) = printf "%s,%s,%s %s" (show rd) (show rn)
    show (RegisterToRegisterArgs rd rm) = printf "%s,%s" (show rd) (show rm)
    show (RegisterShiftedArgs rn rd rs rm st) = printf "%s,%s,%s %s %s" (show rd) (show rn) (show rm) (show st) (show rs)
    show (RegisterShiftedTestArgs rn rs rm st) = printf "%s,%s %s %s" (show rn) (show rm) (show st) (show rs)
    show (RegisterShiftedMvnArgs rd rs rm st) = printf "%s,%s %s %s" (show rd) (show rm) (show st) (show rs)
    show (ImmediateArgs rn rd imm) = printf "%s,%s, #%d" (show rd) (show rn) imm
    show (ImmediateMovArgs rd imm) = printf "%s, #%d" (show rd) imm
    show (RegisterTestArgs rn rm st n) = printf "%s, %s %s" (show rn) (show rm) (show $ Shift (st,n))
    show (RegisterMvnArgs rd rm st n) = printf "%s, %s %s" (show rd) (show rm) (show $ Shift (st,n))
    show (ShiftArgs rd rm n) = printf "%s, %s  #%d" (show rd) (show rm) n
    show (BranchArgs imm) = printf "<PC+%x>" imm
    show (BranchExchangeArgs rm) = (show rm)
    show NoArgs = ""
    show NullArgs = "not parse args"

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

wordToSRType :: Word32 -> SRType
wordToSRType 0 = LSL
wordToSRType 1 = LSR
wordToSRType 2 = ASR
wordToSRType 3 = ROR

wordToRegister :: Word32 -> ArmRegister
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


parseRegister :: InstructionStreamState m => Int -> m ArmRegister
parseRegister off = do 
    wordToRegister <$> instructionBits off 4

instructionFlag :: InstructionStreamState m => Int -> m Bool
instructionFlag off = (`testBit` 0) <$> instructionBits off 1

instructionSignedExtendBits :: InstructionStreamState m => Int -> Int -> m Word32
instructionSignedExtendBits off count = do
    sig <- instructionBits (count - 1) 1
    case sig of 
        0 -> instructionBits off count
        1 -> (.|. complement ((2^count) - 1) ) <$> instructionBits off count

instructionArrayBits :: InstructionStreamState m => [Int] -> m [Word32]
instructionArrayBits = sequence . (fmap (`instructionBits` 1))

