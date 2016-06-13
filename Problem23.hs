import Data.List

smallestAbundantNum = 12
maxNonAbundantPairNum = 28123

half = (`quot` 2)
upToSqrtList num = [2..(truncate (sqrt $ fromIntegral num))]

d `isDivisor` num = (num `mod` d) == 0

properDivisors num = foldl' (properDivisorsHelper num) [1] (upToSqrtList num)
    where properDivisorsHelper num acc val
            | val `isDivisor` num && val == (num `quot` val) = val:acc
            | val `isDivisor` num = val:(num `quot` val):acc
            | otherwise = acc

isAbundant = (!!) (map abundantDef [0..])
    where abundantDef num = sum (properDivisors num) > num

abundantNums = filter isAbundant [smallestAbundantNum..(half maxNonAbundantPairNum)]

isComposedOfAbundantPairSum num = foldl' (anyAbundantSoFar num) False (relevantAbundantNums num)
    where anyAbundantSoFar num acc val = acc || isAbundant (num - val)
          relevantAbundantNums num = takeWhile (<= half num) abundantNums

sumAllNonAbundantPairNums = sum $ filter (not . isComposedOfAbundantPairSum) [1..maxNonAbundantPairNum]
