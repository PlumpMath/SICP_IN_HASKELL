repeated f i = \x -> rep x f i

rep x f i
  | i == 1 = f x
  | otherwise = f nex
      where
        nex = rep x f (i - 1)

square x = x * x


smooth f dx = \x -> (f (x - dx) + f x + f (x + dx)) / 3

