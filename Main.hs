{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString hiding (count)

data Expr
  = Int' Int
  | Var' ByteString
  | Add' Expr Expr
  | Mul' Expr Expr
  | Pow' Expr Expr
  | Ln' Expr
  deriving (Show)

pown :: Int -> Int -> Int
pown a n =
  case n of
    0 -> 0
    1 -> a
    n -> b * b * (if n `mod` 2 == 0 then 1 else a)
          where b = pown a (n `quot` 2)

add, mul, pow :: Expr -> Expr -> Expr

add (Int' m) (Int' n)    = Int' (m + n)
add (Int' 0) f           = f
add f (Int' 0)           = f
add f (Int' n)           = add (Int' n) f
add f (Add' (Int' n) g)  = add (Int' n) (add f g)
add (Add' f g) h         = add f (Add' g h)
add f g                  = Add' f g

mul (Int' m) (Int' n)    = Int' (m * n)
mul (Int' 0) _           = Int' 0
mul _ (Int' 0)           = Int' 0
mul f (Int' n)           = mul (Int' n) f
mul f (Mul' (Int' n) g)  = mul (Int' n) (mul f g)
mul (Mul' f g) h         = mul f (mul g h)
mul f g                  = Mul' f g

pow (Int' m) (Int' n)    = Int' (pown m n)
pow _ (Int' 0)           = Int' 1
pow f (Int' 1)           = f
pow (Int' 0) _           = Int' 0
pow f g                  = Pow' f g

ln :: Expr -> Expr
ln (Int' 1)              = Int' 0
ln f                     = Ln' f

d :: ByteString -> Expr -> Expr
d x (Var' y) | x == y     = Int' 1
d x (Int' _)             = Int' 0
d x (Var' _)             = Int' 0
d x (Add' f g)           = add (d x f) (d x g)
d x (Mul' f g)           = add (mul f (d x g)) (mul g (d x f))
d x (Pow' f g)           = mul (pow f g) (add (mul (mul g (d x f)) (pow f (Int' (-1)))) (mul (ln f) (d x g)))
d x (Ln' f)              = mul (d x f) (pow f (Int' (-1)))

count :: Expr -> Int
count (Int' _) = 1
count (Var' _) = 1
count (Add' f g) = count f + count g
count (Mul' f g) = count f + count g
count (Pow' f g) = count f + count g
count (Ln' f) = count f

prec :: Expr -> Int
prec (Pow' _ _) = 3
prec (Mul' _ _) = 2
prec (Add' _ _) = 1
prec _        = 4

-- this might be nicer in HS
nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = nest (n-1) f (f x)

main :: IO ()
main = do
  let x = Var' "x"
  let f = pow x x
  let res = nest 2 (d "x") f
  print $ res
  --print $ pow (Int' 10) (mul (Int' 10) (Int' 5))
