module LongDiv where

n2digs :: Integral x => x -> [x]
n2digs 0 = []
n2digs x = n2digs (x `quot` 10) ++ [x `rem` 10]

digs2n :: Integral a => [a] -> a
digs2n xs = d (reverse xs) where
    d [] = 0
    d (y:ys) = y + (d ys) * 10

countDigits = length . n2digs

last3 (_,b,c) = c

unpck4 :: Integral a => (a, a, a, a) -> a -> a
unpck4 (a,b,c,d) n | n == 0 = a
                   | n == 1 = b
                   | n == 2 = c
                   | n == 3 = d
                   | otherwise = error "Wrong index!"

longDiv divisor num [] = [(num, n, diff, digit)]
  where 
      digit = num `quot` divisor
      n     = digit * divisor
      diff  = num-n
longDiv divisor num rest = (num, n, diff, digit) : longDiv divisor nextnum nextrest
  where 
      digit = num `quot` divisor
      n     = digit * divisor
      diff  = num - n
      nextnum  = diff * 10 + head rest
      nextrest = tail rest

longDivision dividend divisor = longDiv divisor (num n) (rest n)
    where digits = n2digs dividend
          num n  = digs2n $ take n digits
          rest n = drop n digits
          n | (num l) `quot` divisor /= 0 = l
            | otherwise                   = l+1
            where
                l = countDigits divisor

