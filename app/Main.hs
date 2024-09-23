{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# HLINT ignore "Use and" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}

module Main where

import Prelude hiding (foldr, length, map, product, repeat, sqrt, sum)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (n : list) = n + sum' list

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

product :: (Num a) => [a] -> a
product = foldr (*) 1

anyTrue :: [Bool] -> Bool
anyTrue = or

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

append :: [a] -> [a] -> [a]
append a b = foldr (:) b a

length :: [a] -> Int
length = foldr count 0
 where
  count _ n = n + 1

doubleAll :: [Integer] -> [Integer]
doubleAll = foldr doubleAndCons []
 where
  doubleAndCons n list = n * 2 : list

doubleAll' :: [Integer] -> [Integer]
doubleAll' = foldr ((:) . double) []

doubleAll'' :: [Integer] -> [Integer]
doubleAll'' = map double

double :: (Num a) => a -> a
double n = n * 2

sumMatrix :: [[Int]] -> Int
sumMatrix = sum . map sum

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x [] = x
foldr f x (a : l) = f a (foldr f x l)

data Tree a = Node a [Tree a]

sumTree :: Tree Int -> Int
sumTree = foldTree (+) (+) 0

labels :: Tree a -> [a]
labels = foldTree (:) append []

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (Node . f) (:) []

foldTree :: (a -> b -> c) -> (c -> b -> b) -> b -> Tree a -> c
foldTree f g a (Node label subtrees) = f label (foldSubtrees f g a subtrees)
 where
  foldSubtrees :: (a -> b -> c) -> (c -> b -> b) -> b -> [Tree a] -> b
  foldSubtrees f g a (subtree : rest) = g (foldTree f g a subtree) (foldSubtrees f g a rest)
  foldSubtrees _ _ a [] = a

next :: (Fractional a) => a -> a -> a
next n x = (x + n / x) / 2

repeat :: (t -> t) -> t -> [t]
repeat f a = a : repeat f (f a)

within :: (Ord a, Num a) => a -> [a] -> a
within eps (a : b : rest) =
  if abs (a - b) <= eps then b else within eps (b : rest)

sqrt :: (Ord a, Fractional a) => a -> a -> a -> a
sqrt a0 eps n = within eps (repeat (next n) a0)

relative :: (Ord a, Fractional a) => a -> [a] -> a
relative eps (a : b : rest) =
  if abs (a / b - 1) <= eps then b else relative eps (b : rest)

relativeSqrt :: (Ord a, Fractional a) => a -> a -> a -> a
relativeSqrt a0 eps n = relative eps (repeat (next n) a0)

easyDiff :: (Fractional a) => (a -> a) -> a -> a -> a
easyDiff f x h = (f (x + h) - f x) / h

differentiate :: (Fractional b) => b -> (b -> b) -> b -> [b]
differentiate h0 f x = map (easyDiff f x) (repeat halve h0)
 where
  halve x = x / 2

elimError :: (Fractional a, Integral t) => t -> [a] -> [a]
elimError n (a : b : rest) = ((b * (2 ^ n) - a) / (2 ^ n - 1)) : elimError n (b : rest)

order :: (RealFrac a, Integral b, Floating a) => [a] -> b
order (a : b : c : _) = round (log2 ((a - c) / (b - c) - 1))
 where
  log2 = logBase 2

improve :: (RealFrac a, Floating a) => [a] -> [a]
improve s = elimError (order s) s

diff :: (RealFrac a, Floating a) => a -> a -> (a -> a) -> a -> a
diff eps h0 f x = within eps (improve (differentiate h0 f x))

diff' :: (RealFrac a, Floating a) => a -> a -> (a -> a) -> a -> a
diff' eps h0 f x = within eps (super (differentiate h0 f x))

super :: (RealFrac b, Floating b) => [b] -> [b]
super s = map second (repeat improve s)
 where
  second (_ : b : _) = b

easyIntegrate :: (Fractional a) => (a -> a) -> a -> a -> a
easyIntegrate f a b = (f a + f b) * (b - a) / 2

integrate :: (Fractional a) => (a -> a) -> a -> a -> [a]
integrate f a b = easyIntegrate f a b : map addPair (zip (integrate f a mid) (integrate f mid b))
 where
  mid = (a + b) / 2
  addPair (a, b) = a + b

integrate' :: (Fractional a) => (a -> a) -> a -> a -> [a]
integrate' f a b = integ f a b (f a) (f b)
 where
  integ f a b fa fb = ((fa + fb) * (b - a) / 2) : map addPair (zip (integ f a m fa (f m)) (integ f m b (f m) fb))
  m = (a + b) / 2
  addPair (a, b) = a + b

integrate'' :: (RealFrac b, Floating b) => (b -> b) -> b -> b -> [b]
integrate'' f a b = super (integrate f a b)

main :: IO ()
main = do
  print (sum' [1, 2, 3, 4])
  print (sum [1, 2, 3, 4])
  print (product [1, 2, 3, 4])
  print (anyTrue [False, False, False])
  print (anyTrue [False, True, False])
  print (allTrue [False, True, False])
  print (allTrue [True, True, True])
  print (append [1, 2] [3, 4])
  print (length [10, 9, 8, 7])
  print (doubleAll [10, 9, 8, 7])
  print (doubleAll' [10, 9, 8, 7])
  print (doubleAll'' [10, 9, 8, 7])
  print (sumMatrix [[1, 2], [3, 4], [5, 6]])
  let exTree = Node 1 [Node 2 [], Node 3 [Node 4 []]]
  print (sumTree exTree)
  print (labels exTree)
  print (take 10 $ repeat (next 2) 2.45)
  print (sqrt 2.45 0.0001 2)
  print (relativeSqrt 2.45 0.0001 2)
  print (take 5 $ differentiate 0.0000001 sin (pi / 4))
  print (within 0.00001 $ differentiate 0.0000001 sin (pi / 4))
  print (diff 0.00001 0.000001 sin (pi / 4))
  print (diff' 0.00001 0.000001 sin (pi / 4))
  print (take 10 $ integrate cos 0 (pi / 2))
  print (take 10 $ integrate' cos 0 (pi / 2))
  print (take 10 $ integrate'' cos 0 (pi / 2))
