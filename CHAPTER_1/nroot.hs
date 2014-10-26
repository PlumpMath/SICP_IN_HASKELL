repeated f i = \x -> rep x f i

rep x f i
  | i == 1 = f x
  | otherwise = f nex
      where
        nex = rep x f (i - 1)
tolerance = 0.00001

fixed_point f first_guess = try f first_guess

try f guess
    | close_enough guess next = next
    | otherwise = try f next
        where
        next = f guess


close_enough v1 v2 = abs(v1 - v2) < tolerance

average_damp f = \x -> average x (f x)

average x y = (x + y) / 2

sqrti x = fixed_point (average_damp (\y -> x / y)) 1.0


