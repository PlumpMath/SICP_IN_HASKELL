tolerance = 0.00001

fixed_point f first_guess = try f first_guess

try f guess
    | close_enough guess next = next
    | otherwise = try f next
        where
        next = f guess


close_enough v1 v2 = abs(v1 - v2) < tolerance

           
