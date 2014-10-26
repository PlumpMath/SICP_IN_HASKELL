iden a = a 
inc a = a + 1

accumulate null term a next b
    | a > b = null
    | otherwise = x + (accumulate null term y next b)
        where 
            x = term a
            y = next a
     

