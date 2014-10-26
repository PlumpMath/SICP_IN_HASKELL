func n
   | n < 3 = n
   | otherwise = func (n - 1) + (2 * func (n - 2)) + (3 * func (n - 3))


     
