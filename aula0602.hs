module Main where

import Lib

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

soma :: Int -> Int -> Int
soma a b = a + b

ones :: [Int]
ones = 1 : ones

primeiros :: Int -> [a] -> [a]
primeiros 0 xs = 
    []
primeiros n [] = 
    error "Lista vazia!"
primeiros n (x:xs) =
    x : primeiros (n - 1) xs

main :: IO ()
main = do
    print $ primeiros 4 ones

    -- /ler do input e transformar string em num
    -- a <- getLine
    -- b <- getLine
    -- let n = soma (read a) (read b)
    -- print n
    --