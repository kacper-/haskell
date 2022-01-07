prime :: Int -> IO ()
prime a = do
     print a
     return ()

primes :: Int -> IO ()
primes 0 = return ()
primes a = do
     prime a
     primes (a-1)

main = primes 10

