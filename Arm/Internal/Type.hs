module Arm.Internal.Type (
    wordToSRType, wordToRegister, instructionSignedExtendBits, 
    instructionArrayBits, instructionFlag, parseRegister, 
    parseInstrStream, decodeImmediateShift, parseCondAt, decodeRegisterList,
    wordToEndianState,
    undefinedInstruction,
    InstructionStreamState(..),
    ArmRegister(..),
    ArmInstr(..),
    ArgumentsInstruction(..),
    Cond(..),
    InstrClass(..),
    SRType(..),
    Shift(..),
) where

import Data.Binary
import Data.List
import Text.Printf
import Control.Applicative
import Data.Bits
import Data.Int (Int64)

class (Functor m, Monad m, Applicative m) => InstructionStreamState m where
    instructionBits :: Int -> Int -> m Word32
    parseInstruction :: m ArmInstr
    nextInstruction :: m ()
    instructionOffset :: m Int64
    instructionOpcode :: m Word32

data ArmRegister = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
    deriving (Show)

data ArmInstr = 
          ArmInstr {sectionOffset :: Int64, code :: Word32, cond :: Cond, memonic :: InstrClass, isFlags :: Bool, args :: ArgumentsInstruction  } 
        | NotParsed
        | Undefined Int64 Word32

newtype Shift = Shift (SRType, Word32)

data EndianState = BE | LE

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
            Word32      -- ^ Immediate value to shift
    |   RegisterToRegisterArgs 
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
    |   RegisterMvnArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Word32      -- ^ Immediate value to shift
    |   ImmediateArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rd register
            Word32      -- ^ Immediate value
    |   ImmediateMovArgs
            ArmRegister -- ^ The rd register
            Word32      -- ^ Immediate value
    |   ImmediateTestArgs
            ArmRegister -- ^ The rn register
            Word32      -- ^ The immediate value
    |   ImmediateRelativeArgs -- ^ For adr
            ArmRegister -- ^ The rd register
            Word32      -- ^ The relative immediate value
            Bool        -- ^ Add the relative value or not
    |   RegisterTestArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rm register
            SRType      -- ^ Shift type
            Word32      -- ^ Immediate value to shift
    |   ShiftArgs
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            Word32      -- ^ Immediate value to shift
    |   SaturateArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rd register
            Word32      -- ^ Saturate value
            SRType      -- ^ Shift type
            Word32      -- ^ The value to shift
    |   BranchArgs
            Word32      -- ^ Immediate value to branch
    |   BranchExchangeArgs
            ArmRegister -- ^ The rm register
    |   CompareBranchArgs
            ArmRegister -- ^ The rn register
            Word32      -- ^ The value to branch to
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
    |   LoadLiteralArgs
            ArmRegister -- ^ The rt register
            Word32      -- ^ The immediate value to fetch
            Bool        -- ^ Add immediate value
    |   LoadStoreRegisterArgs
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rt register
            Word32      -- ^ Indicate a LSL shift
    |   LoadStoreImmediateArgs
            ArmRegister -- ^ The rn register
            ArmRegister -- ^ The rt register
            Word32      -- ^ The immediate value offset
            Bool        -- ^ Wback information
            Bool        -- ^ Index information
            Bool        -- ^ Add information
    |   ExtendArgs
            ArmRegister -- ^ The rm register
            ArmRegister -- ^ The rd register
            Word32      -- ^ The rotation
    |   ExtendAddArgs
            ArmRegister -- ^ The rn regsiter
            ArmRegister -- ^ The rd register
            ArmRegister -- ^ The rm register
            Word32      -- ^ Rotation
    |   LoadAndStoreRegisterListArgs -- ^ For stm/ldm
            ArmRegister -- ^ The rn register
            Bool        -- ^ Store or load way back
            [ArmRegister] -- ^ The list of register
    |   RegisterListArgs -- ^ For pop/push
            [ArmRegister] -- ^ The list of register
    |   SettingEndiannessArgs
            EndianState -- ^ BE or LE
    |   ChangeProcessorStateArgs
            Bool        -- ^ True interupt enabled
            Bool        -- ^ Affect the A flag
            Bool        -- ^ Affect the I flag
            Bool        -- ^ Affect the F flag
            Bool        -- ^ True the mode is changed
            Word32      -- ^ The number of mode to change
    |   StoreReturnStateArgs -- ^ For srs{db|ai} instruction
            Bool        -- ^ Direction of the write
            Word32      -- ^ The mode to store
    |   ReturnArgs
            ArmRegister -- ^ The rn register
            Bool        -- ^ wback flag
    |   ItBlockArgs
            Cond        -- ^ The condition that apply
            [Bool]      -- ^ Array for the second third and fourth instruction. Ture=then False=else
    |   NoArgs          -- ^ Instruction with no argument
    |   NullArgs        -- ^ For instruction that is not parsed

{-- This is not used for the time being
    |   ImmediatePcArgs
            ArmRegister -- ^ The rd register
            Word32  -- ^ The immediate value --}

-- | The ARM instruction memonic
data InstrClass = 
    Add | Adc | And | Asr | Adr | 
    Bkpt | Bic | B | Bl | Blx | Bx | Bxj | 
    Clz | Cmp | Cmn | Cbnz | Cbz | Cps | 
    Eor | Eret | 
    Hvc | 
    It |
    Ldr | Ldrsb | Ldrh | Ldrb | Ldrsh | Ldrt | Ldm | Ldmdb | 
    Lsl | Lsr | 
    Mvn | Msr | Mov | Movw | Movs | Movt | Mrs | Mul | 
    Nop |
    Orr | Orn | 
    Push | Pld | Pldw | Pop | Pkh | 
    Rsb | Rsc | Rrx | Ror | Rev | Rev16 | Revsh | Rfedb | Rfeia | 
    Sbc | Smc | Smla | Smlaw | Smulw | Smlal | Sub | Subs | Smul | Setend | 
    Srsdb | Srsia | Str | Strt | Strh | Strht | Strb | Strbt | Stm | Stmdb | Svc | 
    Sxth | Sxtb | Sxtab | Sxtah | Sxtab16 | Sxtb16 | Sev |
    Tst | Teq | 
    Udf | Uxth | Uxtah | Uxtb | Uxtab | Uxtab16 | Uxtb16 |
    Wfe | Wfi |
    Yield
    deriving (Show)

data SRType = ASR | LSL | LSR | ROR | RRX

instance Show ArgumentsInstruction where
    show (RegisterArgs rn rd rm st n) = printf "%s,%s,%s %s" (show rd) (show rn) (show rm) (show $ decodeImmediateShift st n)
    show (RegisterShiftShiftedArgs rd rm rn) = printf "%s,%s,%s" (show rd) (show rn) (show rm)
    show (RegisterToRegisterArgs rd rm) = printf "%s,%s" (show rd) (show rm)
    show (RegisterShiftedArgs rn rd rs rm st) = printf "%s,%s,%s %s %s" (show rd) (show rn) (show rm) (show st) (show rs)
    show (RegisterShiftedTestArgs rn rs rm st) = printf "%s,%s %s %s" (show rn) (show rm) (show st) (show rs)
    show (RegisterShiftedMvnArgs rd rs rm st) = printf "%s,%s %s %s" (show rd) (show rm) (show st) (show rs)
    show (ImmediateArgs rn rd imm) = printf "%s,%s, #%d" (show rd) (show rn) imm
    show (ImmediateMovArgs rd imm) = printf "%s, #%d" (show rd) imm
    show (ImmediateTestArgs rd imm) = printf "%s, #%d" (show rd) imm
    show (RegisterTestArgs rn rm st n) = printf "%s, %s %s" (show rn) (show rm) (show $ decodeImmediateShift st n)
    show (RegisterMvnArgs rd rm st n) = printf "%s, %s %s" (show rd) (show rm) (show $ decodeImmediateShift st n)
    show (ShiftArgs rd rm n) = printf "%s, %s  #%d" (show rd) (show rm) n
    show (BranchArgs imm) = printf "<PC+%x>" imm
    show (BranchExchangeArgs rm) = (show rm)
    show (LoadLiteralArgs rt imm True) = printf "%s, [PC, #%d]" (show rt) imm
    show (LoadLiteralArgs rt imm False) = printf "%s, [PC, #-%d]" (show rt) imm
    show (LoadStoreRegisterArgs rm rn rt 0) = printf "%s, [%s,%s]" (show rt) (show rn) (show rm)
    show (LoadStoreRegisterArgs rm rn rt shift) = printf "%s, [%s,%s lsl #%d]" (show rt) (show rn) (show rm) shift
    show (LoadStoreImmediateArgs rn rt imm False True add) = printf "%s, [%s, %s]" (show rt) (show rn) (showImmediate imm add)
    show (LoadStoreImmediateArgs rn rt imm True True add) = printf "%s, [%s, %s]!" (show rt) (show rn) (showImmediate imm add)
    show (LoadStoreImmediateArgs rn rt imm True False add) = printf "%s, [%s], %s" (show rt) (show rn) (showImmediate imm add)
    show (ExtendArgs rm rd 0) = printf "%s, %s" (show rd) (show rm)
    show (ExtendArgs rm rd rot) = printf "%s, %s, ROR #%d" (show rd) (show rm) rot
    show (ExtendAddArgs rn rd rm 0) = printf "%s, %s, %s" (show rd) (show rn) (show rm)
    show (ExtendAddArgs rn rd rm rot) = printf "%s, %s, %s, ROR #%d" (show rd) (show rn) (show rm) rot
    show (CompareBranchArgs rn imm) = printf "%s, <PC+%x>" (show rn) imm
    show (RegisterListArgs list) = printf "{%s}" (intercalate ", " (map show list))
    show (ChangeProcessorStateArgs effect a i f chmode mode) = printf "%s %s%s" (showeffect effect) (showflag [a,i,f]) (showmode chmode mode)
        where 
            showeffect True = "ie"
            showeffect False = "id"
            selectflag True text = text
            selectflag False text = ""
            showflag lst = intercalate "," (filter (not . null) (zipWith selectflag lst ["A","I","F"]))
            showmode False _ = ""
            showmode True mode = printf ", #%d" mode
    show (LoadAndStoreRegisterListArgs rn wback reglist) = printf "%s%s %s" (show rn) (showwback wback) (intercalate ", " (map show reglist))
        where 
            showwback True = "!"
            showwback False = ""
    show (StoreReturnStateArgs wback mode) = printf "SP%s, #%d" (showwback wback) mode
        where 
            showwback True = "!"
            showwback False = ""
    show (ReturnArgs rn wback) = printf "%s%" (show rn) (showwback wback)
        where 
            showwback True = "!"
            showwback False = ""
    show (ImmediateRelativeArgs rd imm adding) = printf "%s, <PC+#%s%d>" (show rd) (showsign adding) imm
        where 
            showsign True = ""
            showsign False = "-"
    show (SaturateArgs rn rd sat st imm) = printf "%s, #%d, %s%s" (show rd) sat (show rn) (show $ decodeImmediateShift st imm)
    show (ItBlockArgs cond lst) = printf "%s %s" (concat . (map toIt) $ lst) (show cond)
        where 
            toIt True = "t"
            toIt False = "e"
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
    show ArmInstr {sectionOffset=off, code=co, cond=c,memonic=m, isFlags=flgs, args=arguments} = printf "%08X: %08X %s%s %s" off co (show m) (show c) (show arguments)
    show NotParsed = "Unknown"
    show (Undefined sectionOffset code) = printf "%08X: %08X Undefined" sectionOffset code

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

showImmediate :: Word32 -> Bool -> String
showImmediate w True = printf "#%d" w
showImmediate w False = printf "#-%d" w

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

-- | Parse a stream of instruction
parseInstrStream :: InstructionStreamState m =>  Int -> m [ArmInstr]
parseInstrStream 0 = return []
parseInstrStream n = do
    nextInstruction 
    i <- parseInstruction
    (i:) <$> parseInstrStream (n-1)

-- | Decoding of shift information.
-- See section A8.4.3 of thereference manual.
decodeImmediateShift :: SRType -> Word32 -> Shift
decodeImmediateShift LSL imm    = Shift (LSL,imm)
decodeImmediateShift LSR 0      = Shift (LSR,32)
decodeImmediateShift LSR imm    = Shift (LSR,imm)
decodeImmediateShift ASR 0      = Shift (ASR,32)
decodeImmediateShift ASR imm    = Shift (ASR,imm)
decodeImmediateShift ROR 0      = Shift (RRX,1)
decodeImmediateShift ROR imm    = Shift (ROR,imm)

-- | Convert a word to EndianState
wordToEndianState :: Word32 -> EndianState
wordToEndianState 1 = BE
wordToEndianState 0 = LE

-- | Decode the cond field in an instruction stream
parseCondAt :: InstructionStreamState m => Int -> m Cond
parseCondAt off = do
    cond <- instructionBits off 4
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

-- | Return the undefined instruction
undefinedInstruction :: InstructionStreamState m => m ArmInstr
undefinedInstruction = Undefined <$> instructionOffset <*> instructionOpcode
    
-- We should validate that the length of the array is 16.
decodeRegisterList :: [Word32] -> [ArmRegister]
decodeRegisterList = reverse . fst . (foldr regselect ([],0))
    where regselect 0 (r,i) = (r,i+1)
          regselect 1 (r,i) = ((wordToRegister i):r,i+1)
