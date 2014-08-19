module ArmType (
    armInstructionOffset
) 
where 

import Internal.Type
import Data.Int (Int64)

-- | Return the offsset of the arm instruction inside the section
armInstructionOffset :: ArmInstr -> Int64
armInstructionOffset = sectionOffset

