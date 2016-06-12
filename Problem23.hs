import Data.List

smallestAbundantNum = 12
maxNonAbundantPairNum = 28123

half = (`quot` 2)
firstHalfList num = [1..(half num)]
secondHalfList num = [(half num + 1)..(num - 1)]

isDivisor num d = (num `mod` d) == 0

properDivisors num = filter (isDivisor num) (firstHalfList num)

isAbundant = (!!) (map abundantDef [0..])
    where abundantDef num = sum (properDivisors num) > num

abundantNums = filter isAbundant [smallestAbundantNum..(half maxNonAbundantPairNum)]

isComposedOfAbundantPairSum num = foldl' (anyAbundantSoFar num) False (relevantAbundantNums num)
    where anyAbundantSoFar num acc val = acc || isAbundant (num - val)
          relevantAbundantNums num = takeWhile (<= half num) abundantNums

sumAllNonAbundantPairNums = sum $ filter (not . isComposedOfAbundantPairSum) [1..maxNonAbundantPairNum]
