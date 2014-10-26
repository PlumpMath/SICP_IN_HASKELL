tolerance = 0.00001

fixed_point f first_guess = try f first_guess
    
try f guess 
    | close_enough guess next = next
    | otherwise = try f next
        where 
        next = f guess


close_enough v1 v2 = abs(v1 - v2) < tolerance

deriv g = \x -> ((g (x + dx)) - (g x)) / dx

dx = 0.00001

newton_transform g = \x -> x - ((g x) / ((deriv g) x))

newton_method g guess = fixed_point (newton_transform g) guess

cubic a b c = \x -> (x * x * x) + (a * x * x) + (b * x) + c
