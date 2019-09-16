Practical 1: Factoring Numbers
Teodora Musatoiu

Here is a simple method for finding the smallest prime factor of a positive
integer:

> factor :: Integer -> (Integer, Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>    where (q,r) = n `divMod` m

for example

*Main> factor 7654321
(19,402859)

because 

*Main> 19 * 402859
7654321

Repeatedly extracting tyhe smallest factor will return a list
of prime factors:

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>    where (p,q) = factorFrom m n

for example

*Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]

Exercise 1.
factor 0 -> (2,0)
factor 1 -> _ (the process will not stop running)

Exercise 2.
*Main> factor 0
(2,0)
*Main> factor 1


Exercise 3.
Let k be the smallest factor of n. Assume it is smaller than sqrt(n). Therefore, there
must exist a factor p > k, such that k*p = n. But k < n => p*k > k*k > sqrt(n)^2 = n => 
Contradiction. Therefore, the smallest factor of n should be smaller than sqrt(n).

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n

> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0    = (m,q)
>                 | n <= m*m  = (n,1)
>		  		  | otherwise = factorFrom1 (m+1) n
>   	 where (q,r) = n `divMod` m

Yes, the order of the guarded operators matters as conditions will be verified in order. If n <= m*m or ortherwise came first, the condition r==0 will not be checked and the actual result will be lost.

It would take approximatively sqrt(n) recursive calls to evaluate factor1 n in the worst 
case. (This happenes when n is prime.)

Exercise 4.
If a number is prime, then it will not have any prime factors except from itself. If 
the quotient is smaller than the number that you divide by, it means that the factor
becomes bigger than sqrt(n). So the condition is the same. The program will be more 
efficient because q and m are already known, so the computer doesn't have to do any
other opperations to find them. In contrast, for the first condition, the computer should
firstly do m*m and then decide if it is greater or equal to n.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0    = (m,q)
>                 | q <= m    = (n,1)
>		  		  | otherwise = factorFrom2 (m+1) n
>   	 where (q,r) = n `divMod` m

Exercise 5.
This program should be twice as fast, as it only goes through odd numbers.

> factor3 :: Integer -> (Integer, Integer)
> factor3 n = factorFrom3 3 n

> factorFrom3 :: Integer -> Integer -> (Integer, Integer)
> factorFrom3 m n | n `mod` 2 == 0  = (2,n `div` 2)
>		  		  | r == 0          = (m,q)
>                 | q <= m    	    = (n,1)
>		  		  | otherwise 	    = factorFrom3 (m+2) n
>   	 where (q,r) = n `divMod` m

Exercise 6.
*Main> factor3 7
(7,1)
*Main> factor3 6
(2,3)
*Main> factor3 9
(3,3)

Exercise 7.

> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 5 n 2

> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | n `mod` 2 == 0  = (2, n `div` 2)
>		    		| n `mod` 3 == 0  = (3, n `div` 3)
>		    		| r == 0          = (m, q)
>                   | q <= m          = (n, 1)
>		    		| otherwise       = factorFrom4 (m+s) n (6-s)
>    		where (q,r) = n `divMod` m

*Main> factor4 45
(3,15)
*Main> factor4 50
(2,25)
*Main> factor4 55
(5,11)

Exercise 8.
The prime numbers don't have an indentation rule in order to give m only prime values. That
is, a prime number cannot be expressed by a recursive formula.

Exercise 9.
*Main> factors 12
[2,2,3]
*Main> factors 17
[17]
*Main> factors 9
[3,3]

> factors2 :: Integer -> [Integer]
> factors2 n = factorsFrom2 2 n

> factorsFrom2 :: Integer -> Integer -> [Integer]
> factorsFrom2 m n | n == 1    = []
>                  | otherwise = p:(factorsFrom2 p q)
>   	 where (p,q) = factorFrom4 5 n 2

*Main> factors2 12
[2,2,3]
*Main> factors2 17
[17]
*Main> factors2 9
[3,3]

Exercise 10.
*Main> factors 123456789
[3,3,3607,3803]
(0.01 secs, 1,585,936 bytes)
*Main> factors2 123456789
[3,3,3607,3803]
(0.01 secs, 1,057,200 bytes)
*Main> factors 1234567898765
[5,41,25343,237631]
(0.22 secs, 93,250,688 bytes)
*Main> factors2 1234567898765
[5,41,25343,237631]
(0.03 secs, 6,857,336 bytes)


*Main> factors2 8616460799
[89681,96079]
(0.10 secs, 23,610,904 bytes)