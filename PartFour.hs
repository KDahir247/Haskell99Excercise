module PartFour (isPrime,
                    calculateGcd,
                    gcd',
                    comprime,
                    totientPhi,
                    primeFactor,
                    primeFactorMult,
                    primeR,
                    goldbach,
                    goldbachList) where
import Data.List

--31 Determine whether a given integer number is prime.
isPrime ::Int -> Bool
--isPrime v = if v == 2 then True else mod v 2 /= 0 && mod v 3 /= 0
isPrime v = if v == 1 then False else length (filter(\x -> mod v x == 0)  [1..v]) <= 2
 
--32  Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
calculateGcd :: Integral t => t -> t -> t
calculateGcd a b
    | a == 0  = b
    | b == 0 = a
    | otherwise = calculateGcd (b) (mod a b)

gcd' :: Integral p => p -> p -> p
gcd' a b = if a /= 0 && b /= 0 then calculateGcd a b else 0 

--33 Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
comprime :: Integral a => a -> a -> Bool
comprime a b = if (gcd a b <= 1) then True else False

--34 Calculate Euler's totient function phi(m).
totientPhi::Int -> Int
totientPhi v = if isPrime v then v - 1 else length(filter(\x -> comprime x v) [1..v]) 

--35  Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primeFactor::Int -> [Int]
primeFactor v 
    | isPrime v = 1 : v : []
    | otherwise = firstPrimeDivisible : primeFactor (div v firstPrimeDivisible)
        where firstPrimeDivisible = filter (\x -> mod v x == 0) [1..v] !! 1

--36 Determine the prime factors of a given positive integer.
-- sort then group then count the lenght of the inner list then apply nub
primeFactorMult :: Int -> [(Int, Int)]
primeFactorMult v  
    | isPrime v = [(1,1), (v,1)]
    | otherwise =  zip  (map (\x -> length x) (sort $ group $ primeFactor v)) (sort $ nub $ primeFactor v)

--37 Calculate Euler's totient function phi(m) (improved).
--See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
--TODO Not Done


--38 Compare the two methods of calculating Euler's totient function.
--Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.

--No Solution Required

--39 A list of prime numbers.
--Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primeR:: Int -> Int -> [Int]
primeR strt end = filter(\x -> isPrime x) [strt..end]

--40 Goldbach's conjecture.
--Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
goldbach :: Int -> (Int, Int)
goldbach v 
    | v <= 2 = error "Goldbach's conjecture value must greater then 2"
    | odd v = error "Goldbach's conjecture value must be even"
    | otherwise =  (primeList v !! 0, (v - primeList v !! 0)) --note that the even number can be subract by different combination of prime number to result in a prime sum
    where primeList v = filter(\x -> isPrime(v - x) && v-x + x == v) $ filter (\x -> isPrime x) [1..v]

--41 Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
--In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList start end  
    | odd start || start == 2 = goldbachList (start + 1) end
    | start > end = []
    | otherwise = goldbach start : goldbachList (start + 1) end
