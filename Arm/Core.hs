module Arm.Core (
    parseThumbStream, parseArmStream
)
where

import Arm.Internal.Type
import qualified Arm.ThumbDecode as Thumb
import qualified Arm.ArmDecode as Arm
import Control.Applicative
import Control.Monad.State.Lazy
import Data.ByteString

-- | Parse a stream of instruction
parseInstrStream :: InstructionStreamState m =>  Int -> m [ArmInstr]
parseInstrStream 0 = return []
parseInstrStream n = do
    nextInstruction 
    i <- parseInstruction
    (i:) <$> parseInstrStream (n-1)

parseThumbStream :: ByteString -> Int -> [ArmInstr]
parseThumbStream s n = fst (runState (parseInstrStream n) (Thumb.initialState s))

parseArmStream :: ByteString -> Int -> [ArmInstr]
parseArmStream s n = fst (runState (parseInstrStream n) (Arm.initialState s))
