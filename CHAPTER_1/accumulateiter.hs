inc x = x + 1
iden x = x

accum null term a next b 
  | a > b = null
  | otherwise = accum tb term na next b
      where
        na = next a
        tb = term (a + null)


