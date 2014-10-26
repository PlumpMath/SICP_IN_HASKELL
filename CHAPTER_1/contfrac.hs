cont_frac n d k l
  | l == k = n l
  | l == 1 = (n l) / (d (l - 1) + ((n l) / (cont_frac n d k (l + 1))))
  | otherwise = (d (l - 1)) + ((n l) / (cont_frac n d k (l + 1)))

--cont_frac (\x -> 1.0) (\y -> 1.0) 10 1
