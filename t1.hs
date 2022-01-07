maxp :: Int -> Int
maxp x = floor (sqrt (fromIntegral x))

listp :: Int -> [Int]
listp x = [2..(maxp x)]

divp :: Int -> [Int] -> Int
divp x [] = 0
divp x (y:ys) = if (mod x y) == 0 then 1 else divp x ys

testp :: Int -> Int
testp x = divp x (listp x)

printp :: [Int] -> [Int]
printp x = filter (\y -> (testp y) == 0) x

flimit :: Int -> Int
flimit x = div x 2 

flistx :: Int -> [Int]
flistx x = [2..(flimit x)]

buildf :: Int -> [Int] -> [Int] -> [Int]
buildf x [] z = z
buildf x (y:ys) z = if (mod x y) == 0 then buildf (div x y) (y:ys) (z ++ [y]) else buildf x ys z 

factorx :: Int -> [Int]
factorx x = buildf x (printp (flistx x)) []

main = do
  putStrLn "Type number:"
  s <- getLine
  let x = read s :: Int
  print (factorx x)
