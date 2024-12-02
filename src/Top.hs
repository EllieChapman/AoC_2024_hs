module Top where

import Utils
import Day01
import Day02

main :: IO ()
main = do

    putStrLn "*Aoc2024*"

    putStrLn "*Day 1 Part 1*"
    test <- readLinesFromFile "src/Day01_test.txt"
    xs <- readLinesFromFile "src/Day01_input.txt"
    -- print (day1_part1 _xs)
    print (check 11 (day1_part1 test))
    print (check 1320851 (day1_part1 xs))

    putStrLn "*Day 1 Part 2*"
    print (check 31 (day1_part2 test))
    print (check 26859182 (day1_part2 xs))

    -- ebc todo find way to run as test or real, have multiple inout files, maybe directotry per day


    putStrLn "*Day 2 Part 1*"
    test <- readLinesFromFile "src/Day02_test.txt"
    xs <- readLinesFromFile "src/Day02_input.txt"

    print (check 2 (day2_part1 test))
    print (check 490 (day2_part1 xs))

    putStrLn "*Day 2 Part 2*"
    print (check 4 (day2_part2 test))
    print (check 536 (day2_part2 xs))

-- 519 too low