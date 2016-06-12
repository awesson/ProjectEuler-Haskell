half num = num `quot` 2
firstHalfList num = [1..(half num)]
secondHalfList num = [(half num + 1)..(num - 1)]

isDivisor num d = (num `mod` d) == 0

properDivisors num = filter (isDivisor num) (firstHalfList num)

isAbundant = (!!) (map abundantDef [0..])
    where abundantDef num = sum (properDivisors num) > num

secondHalfListWDup num
    | odd num = secondHalfList num
    | otherwise = half num : secondHalfList num
pairSums num = zip (firstHalfList num) (reverse $ secondHalfListWDup num)

containsAbundantPair ((n1, n2):xs)
    | not (isAbundant n1) || not (isAbundant n2) = containsAbundantPair xs
    | otherwise = True
containsAbundantPair [] = False
isComposedOfAbundantPairSum num = containsAbundantPair $ pairSums num

maxNonAbundantPairNum = 1000
sumAllNonAbundantPairNums = sum $ filter (not . isComposedOfAbundantPairSum) [1..maxNonAbundantPairNum]
