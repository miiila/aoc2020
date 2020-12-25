import Data.List

main = do
    let cardPublicKey = 1526110
        doorPublicKey = 20175123
        cardPublicKey_test = 5764801
        doorPublicKey_test = 17807724
    -- Part 1
        transformSeven = transformSubjectNumber 7
        cardLoopSize = length $ takeWhile(/= cardPublicKey) $ iterate transformSeven 1
        doorLoopSize = length $ takeWhile(/= doorPublicKey) $ iterate transformSeven 1
    print $ iterate' (transformSubjectNumber doorPublicKey) 1 !! cardLoopSize
    -- Part 2

transformSubjectNumber:: Integer -> Integer -> Integer
transformSubjectNumber subjectNumber val = (val * subjectNumber) `mod` 20201227 
