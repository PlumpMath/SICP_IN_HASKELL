smallestdivisor n = finddivisor n 2

divides a b = mod b a == 0

next testdivisor
    | testdivisor == 2 = 3
    | otherwise = testdivisor + 2

finddivisor n testdivisor
    | (testdivisor * testdivisor) > n = n
    | (divides testdivisor n) = testdivisor
    | otherwise = finddivisor n (next testdivisor)

prime n = n == smallestdivisor n
