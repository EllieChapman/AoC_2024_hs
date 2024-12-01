module Day01 where

import Data.List

day1_part1 :: [String] -> Int
day1_part1 xs = do
    let l1 = sort (parse1 xs [])
    let l2 = sort (parse2 xs [])
    comp l1 l2 0

day1_part2 :: [String] -> Int
day1_part2 xs = do
    let l1 = sort (parse1 xs [])
    let l2 = sort (parse2 xs [])
    similarity l1 l2 0

parse1 :: [String] -> [Int] -> [Int]
parse1 xs is = do
    case xs of
        x:xs -> do
            parse1 xs (is ++ [read (head (words x))])
        [] -> is

parse2 :: [String] -> [Int] -> [Int]
parse2 xs is = do
    case xs of
        x:xs -> do
            parse2 xs (is ++ [read (last (words x))])
        [] -> is

-- ebc todo, consider making own safe versions of head and last, or general utils functions

comp :: [Int] -> [Int] -> Int -> Int
comp is1 is2 total = do
    case is1 of
        i1:is1 -> do
            case is2 of
                i2:is2 -> comp is1 is2 (total + (comp_pair i1 i2))
                [] -> error "blah"
        [] -> total



comp_pair :: Int -> Int -> Int
comp_pair i1 i2 =
    if i1 > i2
    then i1 - i2
    else i2 - i1

similarity :: [Int] -> [Int] -> Int -> Int
similarity l1 l2 total = do
    case l1 of
        i:is -> similarity is l2 (total + (calculate i l2))
        [] -> total

calculate :: Int -> [Int] -> Int
calculate i xs = i * length (filter (\n -> n == i) xs)