smallest_divisor n = find_divisor n 2

find_divisor n test_divisor
    | (test_divisor * test_divisor) > n = n
    | divides test_divisor n = test_divisor
    | otherwise = find_divisor n (test_divisor + 1)

divides a b = mod b a == 0

prime n = n == smallest_divisor n

inc x = x+1
iden x = x

accumulate null term a next b filt
  | a > b = null
  | filt a = (term a) + acc
  | otherwise = null +acc
      where
          acc = accumulate null term d next b filt
          d = next a

