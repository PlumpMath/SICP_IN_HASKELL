cube x = x * x * x

inc a = a + 1
 
toggle m
   | m == 1 = 0
   | m == 0 = 1

sum1 a next b m term
    | a > b = 0
    | otherwise = te + sum1 ne next b ml term
        where
            te = term a ml
            ne = next a
            ml = toggle m

  
simpson f a b n = h / 3  * (sum1 0 inc n 1 (\x m -> new x m f a h n))
                    where h = (b - a) / n
                          


new x m f a h n
    | x == 0 = f a + (x * h)
    | x == n = f (a + (x * h))
    | m == 1 = (4 * f (a + (x * h)))
    | otherwise = 2 * f (a + (x * h))




                        

