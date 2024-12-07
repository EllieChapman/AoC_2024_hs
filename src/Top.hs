module Top where

import Utils
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07

-- import Data.Map.Strict as M

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

    test <- readLinesFromFile "src/Day04_test.txt"
    xs <- readLinesFromFile "src/Day04_input.txt"
    putStrLn "\n*Day 4 Part 1*"
    day4_part1_test <- day4_part1 test
    day4_part1_xs <- day4_part1 xs
    print (check 18 (day4_part1_test))
    print (check 2454 (day4_part1_xs))
    putStrLn "*Day 4 Part 2*"
    day4_part2_test <- day4_part2 test
    -- day4_part2_xs <- day4_part2 xs
    print (check 9 (day4_part2_test))
    -- print (check 1858 (day4_part2_xs))
    print "part 2 too slow!"

    test <- readLinesFromFile "src/Day05_test.txt"
    xs <- readLinesFromFile "src/Day05_input.txt"
    putStrLn "\n*Day 5 Part 1*"
    print (check 143 (day5_part1 test))
    print (check 6034 (day5_part1 xs))
    putStrLn "*Day 5 Part 2*"
    day5_part2_test2 <- day5_part2 test
    -- day5_part2_xs <- day5_part2 xs
    print (check 123 (day5_part2_test2))
    -- print (check 6305 (day5_part2_xs)) --super slow!
    print "part 2 too slow!"


    test <- readLinesFromFile "src/Day06_test.txt"
    xs <- readLinesFromFile "src/Day06_input.txt"
    putStrLn "\n*Day 6 Part 1*"
    day6_part1_test <- day6_part1 test
    day6_part1_xs <- day6_part1 xs
    print (check 41 day6_part1_test)
    print (check 5453 day6_part1_xs)
    putStrLn "*Day 6 Part 2*"
    day6_part2_test <- day6_part2 test
    -- day6_part2_xs <- day6_part2 xs
    print (check 6 (day6_part2_test))
    -- print (check 2188 (day6_part2_xs)) --super slow!
    print "part 2 too slow!"

    test <- readLinesFromFile "src/Day07_test.txt"
    xs <- readLinesFromFile "src/Day07_input.txt"
    putStrLn "\n*Day 7 Part 1*"
    day7_part1_test <- day7_part1 test
    day7_part1_xs <- day7_part1 xs
    print (check 3749 day7_part1_test)
    print (check 42283209483350 day7_part1_xs)
    putStrLn "*Day 7 Part 2*"
    day7_part2_test <- day7_part2 test
    day7_part2_xs <- day7_part2 xs
    print (check 11387 (day7_part2_test))
    print (check 1026766857276279 (day7_part2_xs))

