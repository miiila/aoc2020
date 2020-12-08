import Data.List
import Data.Char
import qualified Data.Set as Set

main = do
    contents <- readFile "day8_input"
    let input = lines contents
    -- Part 1
        program = parseInstruction <$> input
        res = runProgram program 0 (Set.fromList []) 0 (length input)
    print res
    -- Part 2
    let jmpsNops = sort $ findIndices (\x -> fst x == Jmp || fst x == Nop) program
        programs = runProgram <$> (modifyProgram <$> repeat program <*> jmpsNops)
        res = find fst $ programs <*> [0] <*> [Set.fromList []] <*> [0] <*> [length input]
    print res


modifyProgram::[Operation] -> Int -> [Operation]
modifyProgram program insChanged =
    let (start, ins:rest) = splitAt insChanged program
        in start ++ newIns ins:rest
       where newIns (Nop, x) = (Jmp, x)
             newIns (Jmp, x) = (Nop, x)


runProgram::[Operation] -> Int -> Set.Set Int -> Int -> Int -> (Bool, Int)
runProgram program acc used current targetInstructionNumber
 | Set.member current used = (False, acc)
 | current == targetInstructionNumber = (True, acc)
 | otherwise =
     let (newAcc, newCurrent) = runOperation (program !! current) acc current
         newUsed = Set.insert current used
      in runProgram program newAcc newUsed newCurrent targetInstructionNumber


runOperation::Operation -> Int -> Int -> (Int, Int)
runOperation (Nop, _) acc next = (acc, next + 1)
runOperation (Acc, val) acc next = (acc + val , next + 1)
runOperation (Jmp, val) acc next = (acc , next + val)

type Operation = (Instruction, Int)
data Instruction = Nop | Acc | Jmp deriving (Read, Show, Eq, Enum)
parseInstruction::String->Operation
parseInstruction s =
    let [ins,num] = words s
        signedNum = if head num == '-' then num else tail num
        insC = toUpper (head ins) : tail ins
     in (read insC :: Instruction, read signedNum :: Int)
