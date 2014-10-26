iden x = x
inc x = x + 1

product1 term a next b
    | a > b = 1
    | otherwise = x * (product1 term y next b)
         where 
             x = term a
             y = next a
