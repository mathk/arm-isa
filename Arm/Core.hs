module Arm.Core (
    parseThumbStream, parseArmStream
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

-- | Parse a stream of instruction
parseInstrStream :: InstructionStreamState m =>  Int -> m [ArmInstr]
parseInstrStream 0 = return []
parseInstrStream n = do
    nextInstruction 
    i <- parseInstruction
    (i:) <$> parseInstrStream (n-1)

parseInstrStreamWhile :: InstructionStreamState m => (ArmInstr -> Bool) -> m [ArmInstr]
parseInstrStreamWhile test = do
    nextInstruction
    i <- parseInstruction
    if test i 
    then return <$> return i
    else (i:) <$> parseInstrStreamWhile test

isBlockEnding :: ArmInstr -> Bool
isBlockEnding ArmInstr {memonic=B} = True
isBlockEnding ArmInstr {memonic=Bx} = True
isBlockEnding ArmInstr {memonic=Bxj} = True
isBlockEnding ArmInstr {memonic=Bl} = True
isBlockEnding ArmInstr {memonic=Pop, args=(RegisterListArgs lst)} = PC `elem` lst
isBlockEnding ArmInstr {memonic=Bxj} = True
isBlockEnding ArmInstr {memonic=Bxj} = True
isBlockEnding _ = False


parseThumbStream :: B.ByteString -> Int -> [ArmInstr]
parseThumbStream s n = fst (runState (parseInstrStream n) (Thumb.initialState s))

parseArmStream :: B.ByteString -> Int -> [ArmInstr]
parseArmStream s n = fst (runState (parseInstrStream n) (Arm.initialState s))
