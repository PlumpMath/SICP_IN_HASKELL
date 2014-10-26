tan_cf x k l
    | l == k = x * x
    | l == 1 = x / tan_cf x k (l + 1)
    | otherwise = l - (x * x) / tan_cf x k (l + 1)
