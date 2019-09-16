-- Digital Systems practical A
-- Mike Spivey, Hilary Term, 2004
-- Geraint Jones, Trinity Term 2011, 2012, 2013
-- Hanno Nickau 2014 - 2018

module Adder where

import System.Random
import Test.QuickCheck

-- representing signals with delays

type Timed a = (a, Int)

instant x = (x, 0)

time :: Timed a -> Int 
time = snd

maxtime :: [Timed a] -> Int
maxtime = maximum . map time

-- Representing integers

type Bit = Timed Int

rep :: Int -> Integer -> [Bit]
rep 0 _ = []
rep n x = (fromInteger (x`mod`2),0) : rep (n-1) (x`div`2)

bin :: [Bit] -> Integer
bin [] = 0
bin xs = toInteger ((fst x))*2^((length xs)-1) + toInteger(bin (init xs))
         where x = last xs

test_bin_rep :: IO()
test_bin_rep = quickCheck (forAll positive $ \n ->
                            forAll nonnegative $ \x ->
                              length (rep n x) == n && 
                              bin (rep n x) == x `mod` 2^n)

-- Useful gates

maj :: (Bit, Bit, Bit) -> Bit
maj ((a,ta), (b,tb), (c,tc)) 
		| (a==1&&b==1) || (a==1&&c==1) || (b==1&&c==1) = (1, maximum [ta,tb,tc] +1)
		| otherwise 								   = (0, maximum [ta,tb,tc] +1)

parity :: (Bit, Bit, Bit) -> Bit
parity ((a,ta), (b,tb), (c,tc)) = ((a+b+c)`mod`2 , maximum [ta,tb,tc] +1)

full_adder :: Bit -> (Bit, Bit) -> (Bit, Bit)
full_adder cin (a, b) = (parity(cin,a,b), maj(cin,a,b))

-- Ripple-carry adder

type Adder = [Bit] -> [Bit] -> [Bit]

adder :: Adder
adder as bs = ripple full_adder (instant 0) (zip as bs)

-- The time grows linearly as you increase n.

ripple :: (a -> b -> (c, a)) -> a -> [b] -> [c]
ripple _ _ [] = []
ripple f a (x:xs) = fst(f a x): ripple f (snd(f a x)) xs

test_rip_full_adder :: IO()
test_rip_full_adder = test_GenAdder positive adder

test_GenAdder length adder 
   = quickCheck (forAll length $ \n ->
                  forAll nonnegative $ \x ->
                   forAll nonnegative $ \y ->
                      bin(adder (rep n (x `mod` 2^n)) 
                                (rep n (y `mod` 2^n))) 
                                      == (x + y) `mod` 2^n)

-- Introducing carry status

data KPG = K | P | G
  deriving (Show,Eq)
type Flag = Timed KPG

kpg :: (Bit, Bit) -> Flag
kpg ((a,ta), (b,tb)) 
		| a == 0 && b == 0 = (K, max ta tb +1)
		| a == 1 && b == 1 = (G, max ta tb +1)
		| otherwise		   = (P, max ta tb +1)

sumbit :: Flag -> (Bit, Bit) -> Bit
sumbit (w,tw) (a, b) 
		| w == K = parity (a,b, (0,tw))
		| w == G = parity (a,b, (1,tw))

bun :: Flag -> Flag -> Flag
(w,tw) `bun` (u,tu) 
		| u == K = (K, max tw tu +1)
		| u == P = (w, max tw tu +1)
		| u == G = (G, max tw tu +1)

rip_kpg_adder :: Adder
rip_kpg_adder as bs = ripple kpg_adder (instant K) (zip as bs)

-- The adder has the same order of growth as the full adder.

kpg_adder :: Flag -> (Bit, Bit) -> (Bit, Flag)
kpg_adder w (a, b) = (sumbit w (a,b) , w `bun` (kpg (a,b)) )

test_rip_kpg_adder :: IO()
test_rip_kpg_adder = test_GenAdder positive rip_kpg_adder


-- Parallel prefix.

type Prefix_fun a = (a -> a -> a) -> a -> [a] -> [a]

cl_adder :: Prefix_fun Flag -> Adder
cl_adder pfx as bs = zipWith sumbit carries inputs
  where 
    carries = pfx bun (instant K) (map kpg inputs)
    inputs = zip as bs

-- This adder operates in 2*logn time. So, the time will be asymptotically bounded
-- by O(log n).

rip_prefix :: Prefix_fun a
rip_prefix _ _ [] = []
rip_prefix (*) a (x:xs) = a:(rip_prefix (*) (a*x) xs)

test_rip_cl_adder = test_GenAdder positive (cl_adder rip_prefix)

par_prefix :: Prefix_fun a
par_prefix (*) a [x] = [a]
par_prefix (*) a xs = concat (zipWith f es (par_prefix (*) a (zipWith (*) es os)))
		where es = evens xs; os = odds xs; f x y = [y, y * x]

evens, odds :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs
odds [] = []
odds (_:xs) = evens xs

test_par_cl_adder :: IO()
test_par_cl_adder = test_GenAdder powerOfTwo (cl_adder par_prefix)

--
-- QuickCheck library for running some random checks
--


test_all  = do { putStr "bin and rep:       "; test_bin_rep;
                 putStr "Ripple Full Adder: "; test_rip_full_adder;
                 putStr "Ripple KPG Adder:  "; test_rip_kpg_adder;
                 putStr "Ripple CL Adder:   "; test_rip_cl_adder;
                 putStr "Parallel CL Adder: "; test_par_cl_adder}

--
-- Random test case generators
--

-- generator for  a >= 0
nonnegative :: (Integral a, Random a) => Gen a
nonnegative = sized $ \n -> choose (0, fromIntegral n)

-- generator for  a > 0
positive :: (Integral a, Random a) => Gen a
positive = sized $ \n -> choose (1, fromIntegral n)


-- generator for small powers of two
powerOfTwo :: Gen Int
powerOfTwo =  sized $ \n -> elements [2^i | i <- [0..14], i <= n]

--
-- marker for parts of the code that need to be implemented
--

to_do :: String -> a
to_do f = error (f ++ " not yet implemented")
