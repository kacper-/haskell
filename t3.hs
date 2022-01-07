import Text.Printf
import Control.Exception
import System.CPUTime

maxp x = floor (sqrt (fromIntegral x))
listp x = [2..(maxp x)]
divp x [] = 0
divp x (y:ys) = if (mod x y) == 0 then 1 else divp x ys
testp x = divp x (listp x)
printp x = map show ( filter (\y -> (testp y) == 0) x )
main = do
  start <- getCPUTime
  let a = (printp [1970..1999])
  mapM_ putStrLn a
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.6f sec\n" (diff :: Double)


