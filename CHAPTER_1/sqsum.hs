sq a b c
   | a > b = if  b > c then a * a * b * b else a * a * c * c
   | otherwise = if a > c then a * a * b * b else c * c * b * b
