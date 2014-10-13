module Arm.ArmType (
    instructionBlockOffset
) 
where 

import Arm.Internal.Type
import Data.Int (Int64)

-- | Return the offsset of the arm instruction inside the section
instructionBlockOffset :: ArmInstr -> Int64
instructionBlockOffset = blockOffset

