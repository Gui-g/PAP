module Main where

import Lib
import Data.Char

concatenacao :: [a] -> [a] -> [a]
concatenacao xs [] = xs
concatenacao [] ys = ys
concatenacao (x:xs) ys = x : concatenacao xs ys

pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence n (x:xs)
    | x == n = True
    | otherwise = pertence n xs

intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] _ = []
intersecao (x:xs) ys
    | pertence x ys = x : intersecao xs ys
    | otherwise = intersecao xs ys

inverso :: [a] -> [a] 
inverso [] = [] 
inverso (x:xs) = concatenacao (inverso xs) [x]

primeiros :: Int -> [a] -> [a]
primeiros 0 _ = []
primeiros n [] = error "Lista com menos elementos do que o pedido"
primeiros n (x:xs) = x : primeiros (n - 1) xs

ultimos :: Int -> [a] -> [a]
ultimos 0 _ = []
ultimos n xs = inverso $ primeiros n $ inverso xs

binParaInt :: String -> Int
binParaInt [] = 0
binParaInt (x:xs) = digitToInt x * (2 ^ length xs) + binParaInt xs

intParaBin :: Int -> String
intParaBin 0 = ""
intParaBin n = concatenacao (intParaBin $ div n 2) (show $ rem n 2)

menorValor :: Ord a => [a] -> a
menorValor [] = error "Lista Vazia"
menorValor [n] = n
menorValor (x:y:xs) = if x < y then menorValor(x:xs) else menorValor(y:xs)

removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro [] _ = error "Lista Vazia"
removerPrimeiro (x:xs) n
    | x == n = xs
    | otherwise = x : removerPrimeiro xs n

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs)
    | x == menorValor (x:xs) = x : ordenar xs
    | otherwise = ordenar $ concatenacao xs [x]

dobrarDir :: (a -> b -> b) -> b -> [a] -> b
dobrarDir f z [] = z
dobrarDir f z (x:xs) = f x (dobrarDir f z xs)

dobrarEsq :: (b -> a -> b) -> b -> [a] -> b
dobrarEsq f z [] = z
dobrarEsq f z (x:xs) = dobrarEsq f (f z x) xs

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar f (x:xs)
    | f x == True = x : filtrar f xs
    | otherwise = filtrar f xs

impares :: [Int] -> [Int]
impares [] = []
impares xs = filtrar (impar) xs
   where impar x = mod x 2 == 1

mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

primeirosTupla :: [(a,b)] -> [a]
primeirosTupla [] = []
primeirosTupla x = mapear fst x

todos :: [Bool] -> Bool
todos [] = True
todos xs = dobrarDir (&&) True xs

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

maior :: Ord a => Tree a -> a
maior (Leaf a) = a
maior (Branch l r) = max (maior l) (maior r)

altura :: Tree a -> Int 
altura (Leaf a) = 0
altura (Branch l r) = 1 + max (altura l) (altura r)

main :: IO ()
main = do
    let lista_a = [5, 4, 3, 2, 1]
    let lista_b = [5, 6, 7, 8, 9]
    let resultado_01 = concatenacao lista_a lista_b
    print resultado_01

    let resultado_02a = pertence 7 lista_a
    print resultado_02a
    let resultado_02b = pertence 1 lista_a
    print resultado_02b

    let resultado_03 = intersecao lista_a lista_b
    print resultado_03

    let resultado_04 = inverso lista_a
    print resultado_04

    let resultado_05 = primeiros 2 lista_a
    print resultado_05
    
    let resultado_06 = ultimos 2 lista_a
    print resultado_06

    let bin_mock = "11101"
    let resultado_07 = binParaInt bin_mock
    print resultado_07

    let int_mock = 29
    let resultado_08 = intParaBin int_mock
    print resultado_08

    let resultado_09a = menorValor lista_a
    print resultado_09a
    let resultado_09b = menorValor lista_b
    print resultado_09b

    let resultado_10 = removerPrimeiro lista_a 3
    print resultado_10

    let resultado_11 = ordenar lista_a
    print resultado_11

    let resultado_12 = dobrarDir (-) 0 lista_a
    print resultado_12

    let resultado_13 = dobrarEsq (-) 0 lista_a
    print resultado_13

    let resultado_14 = filtrar (>= 3) lista_a
    print resultado_14

    let resultado_15 = impares lista_a
    print resultado_15

    let resultado_16 = mapear (^2) lista_a
    print resultado_16

    let tuple_list = [(1,0),(2,0),(3,0),(4,0),(5,0)]
    let resultado_17 = primeirosTupla tuple_list
    print resultado_17

    let bool_list_a = [True, True, False, True]
    let bool_list_b = [True, True, True, True]
    let resultado_18a = todos bool_list_a
    print resultado_18a
    let resultado_18b = todos bool_list_b
    print resultado_18b

    let mock_tree = Branch (Branch (Branch (Leaf 5) (Leaf 3)) (Leaf 2)) (Leaf 18)
    let resultado_19 = maior mock_tree
    print resultado_19

    let resultado_20 = altura mock_tree
    print resultado_20