import Data.List
import qualified Data.IntSet as IntSet

smallestAbundantNum = 12
maxNonAbundantPairNum = 28123

upToSqrtList num = [2..(truncate (sqrt $ fromIntegral num))]

d `isDivisor` num = (num `mod` d) == 0

-- Check if each number less than the sqrt evenly divides into the number.
properDivisors num = foldl' (properDivisorsHelper num) [1] (upToSqrtList num)
    where properDivisorsHelper num acc val
            -- Add the pair of divisors when it does,
            -- unless the pair is the sqrt in which case just add it once.
            | val `isDivisor` num && val == (num `quot` val) = val:acc
            | val `isDivisor` num = val:(num `quot` val):acc
            | otherwise = acc

-- Use the index in a map in order to make this a CAF and get cached
isAbundant = (!!) (map abundantDef [0..])
    where abundantDef num = sum (properDivisors num) > num

abundantNums = filter isAbundant [smallestAbundantNum..maxNonAbundantPairNum]

-- Essentially a loop over each abundant number and then an inner loop that
-- pairs the current number with each abundant number less than it,
-- including itself. This will give each unique pair of abundant numbers,
-- which then get's summed together and added to an IntSet to remove duplicates
-- and make queries fast.
abundantPairNums = foldl' innerLoop IntSet.empty abundantNums
    where innerLoop acc val = foldl' (insertSum val) acc (takeWhile (<= val) abundantNums)
            where insertSum outerNum acc val = IntSet.insert (val + outerNum) acc

sumAllNonAbundantPairNums = sum $ filter (`IntSet.notMember` abundantPairNums) [1..maxNonAbundantPairNum]
