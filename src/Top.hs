module Top where

import Utils
import Day01
import Day02
import Day03
import Day03part2

main :: IO ()
main = do

    -- ebc todo find way to run as test or real, have multiple inout files, maybe directotry per day

    putStrLn "*Aoc2024*"

    test <- readLinesFromFile "src/Day01_test.txt"
    xs <- readLinesFromFile "src/Day01_input.txt"
    putStrLn "\n*Day 1 Part 1*"
    print (check 11 (day1_part1 test))
    print (check 1320851 (day1_part1 xs))
    putStrLn "*Day 1 Part 2*"
    print (check 31 (day1_part2 test))
    print (check 26859182 (day1_part2 xs))

    test <- readLinesFromFile "src/Day02_test.txt"
    xs <- readLinesFromFile "src/Day02_input.txt"
    putStrLn "\n*Day 2 Part 1*"
    print (check 2 (day2_part1 test))
    print (check 490 (day2_part1 xs))
    putStrLn "*Day 2 Part 2*"
    print (check 4 (day2_part2 test))
    print (check 536 (day2_part2 xs))

    test <- readLinesFromFile "src/Day03_test.txt"
    test2 <- readLinesFromFile "src/Day03_test2.txt"
    xs <- readLinesFromFile "src/Day03_input.txt"
    putStrLn "\n*Day 3 Part 1*"
    print (check 161 (day3_part1 test))
    print (check 187833789 (day3_part1 xs))
    putStrLn "*Day 3 Part 2*"
    print (check 48 (day3_part2 test2))
    print (check 94455185 (day3_part2 xs))

-- 181198196 toonot
-- 97728793 too high
-- 94455185