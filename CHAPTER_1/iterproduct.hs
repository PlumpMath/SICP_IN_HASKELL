inc x = x + 1
identity x = x

iterproduct null term a next b 
  | a > b = null
  | otherwise = iterproduct aw term ae next b
      where 
          aw = term (a * null)
          ae = next a
  
