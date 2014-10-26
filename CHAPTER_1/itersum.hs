inc x = x+1

identity x = x

sum1 null term a next b
   | a > b = null
   | otherwise = sum1 res term te next b
       where 
           te = next a
           res = term (a + null)
