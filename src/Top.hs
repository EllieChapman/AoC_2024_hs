module Top where

import Utils
import Day01

main :: IO ()
main = do

    putStrLn "*Aoc2024*"

    putStrLn "*Day 1 Part 1*"
    xs <- readLinesFromFile "src/Day01_input.txt"
    test <- readLinesFromFile "src/Day01_test.txt"
    -- print (day1_part1 _xs)
    print (check 11 (day1_part1 test))
    print (check 1320851 (day1_part1 xs))

    putStrLn "*Day 1 Part 2*"
    print (check 31 (day1_part2 test))
    print (check 26859182 (day1_part2 xs))

    -- ebc todo find way to run as test or real, have multiple inout files, maybe directotry per day
