type Instruction = String
type Memory = String

isCorrupted :: Instruction -> Bool

scanMemory :: Memory -> String -> (Instruction -> Bool) -> [Instruction]

