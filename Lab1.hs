import Test.QuickCheck
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--Part 1
{-  The number of steps depend on k, 
    k+1 computing steps are required.   -}
        
--Part 2
power1 :: Integer -> Integer -> Integer
power1 n k  | k < 0 = error "power: negative argument"
power1 n k  = product[n | i<-[1..k]]

--Part 3
power2 :: Integer -> Integer -> Integer
power2 n k  | k < 0 = error "power: negative argument"
power2 n 0  = 1
power2 n k  | even k = power2 (n*n) (div k 2)
power2 n k  | otherwise = n * (power2 n (k-1))

--Part 4
{-  We want to test (n=2, k=2), (2, 4) , (4, 2),
    (3, 0), (0, 5), (10, 20) , (-3, 2), (-2, 0) 
    The functions are not defined for negative exponents
    so we do not test any -}
    
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = and[(power n k) == (power1 n k), (power1 n k) == (power2 n k)]

test_powers = and[(prop_powers 2 2), (prop_powers 2 4), (prop_powers 4 2),
    (prop_powers 3 0), (prop_powers 0 5), (prop_powers 10 20), 
    (prop_powers (-3) 2), (prop_powers (-2) 0)] 

-- If we force k to be only positive numbers it should work.
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = and[(power n k') == (power1 n k'), (power1 n k') == (power2 n k')]
            where k' = abs k

