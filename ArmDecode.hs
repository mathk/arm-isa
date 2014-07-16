module ArmDecode () where

import qualified Data.Binary.Bits.Get as Bin
import Data.Binary
import Data.Binary.Bits 


data ArmRegister = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

data ArmInstr = DRI {dri :: DataRegisterInstr }

data Cond = CondEQ | CondNE | CondCS | CondCC | CondMI | CondPL | CondVS | CondVC | CondHI | CondLS | CondGE | CondLT | CondGT | CondLE | CondAL

data DataRegisterInstr = DRInstr { cond :: Cond, op :: Int, op2, imm5 :: Int, instr :: Int, rn :: ArmRegister, rd :: ArmRegister}


parseRegister :: Bin.BitGet ArmRegister
parseRegister = do 
    r <- Bin.getWord8 4
    case  r of
        0 -> return R0
        1 -> return R1
        2 -> return R2
        3 -> return R3
        4 -> return R4
        5 -> return R5
        6 -> return R6
        7 -> return R7
        8 -> return R8
        9 -> return R9
        10 -> return R10
        11 -> return R11
        12 -> return R12
        13 -> return R13
        14 -> return R14
        15 -> return R15

parseCond :: Bin.BitGet Cond
parseCond = do
    cond <- Bin.getWord8 4
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

parseArmInstruction :: Bin.BitGet ArmInstr
parseArmInstruction = do
    condition <- parseCond
    return DRI {dri=DRInstr {cond=condition}}
    



