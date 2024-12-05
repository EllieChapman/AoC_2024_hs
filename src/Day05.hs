module Day05 where
import Data.List.Split



-- day5_part1 :: [String] -> [[Int]]
day5_part1 :: [String] -> Int
day5_part1 _xs = do
    let rules :: [[Int]] = map (\n -> map read (splitOn "|" n)) (get_rule_lines _xs [])
    let updates :: [[Int]] = map (\n -> map read (splitOn "," n)) (drop ((length rules) + 1) _xs)
    -- (map (\n -> test_update n rules) updates)
    sum (map (\n -> test_update n rules) updates)
    -- rules


day5_part2 :: [String] -> Int
day5_part2 _xs = do
    let rules :: [[Int]] = map (\n -> map read (splitOn "|" n)) (get_rule_lines _xs [])
    let updates :: [[Int]] = map (\n -> map read (splitOn "," n)) (drop ((length rules) + 1) _xs)
    let bad_updates :: [[Int]] = get_bad updates [] rules
    let fixed_bad :: [[Int]] = map (\n -> fix_bad n rules) bad_updates
    sum (map middle fixed_bad)


get_rule_lines :: [String] -> [String] -> [String]
get_rule_lines to_check found = do
    case to_check of
        x:xs -> do
            case x of
                _:_ -> get_rule_lines xs (x:found)
                [] -> found
        [] -> error "this should not reach the end of input lines"


get_bad :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
get_bad updates found rules =
    -- undefined updates found rules
    case updates of
        [] -> found
        u:us -> do
            if test_update u rules == 0
            then get_bad us (u:found) rules
            else get_bad us found rules

fix_bad :: [Int] -> [[Int]] -> [Int]
fix_bad bad rules = do
    undefined bad rules




-- either returns middle page num or 0
-- test_update :: [Int] -> [[Int]] -> [Int]
test_update :: [Int] -> [[Int]] -> Int
test_update update rules = do
    -- (map (\n -> check_rule update n) rules)
    let score = sum (map (\n -> check_rule update n) rules)
    if score == 0
    -- then error "here"
    then middle update
    else 0

middle :: [Int] -> Int
middle is = do
    head (drop ((length is) `div` 2) is)

-- 0 if good or doesnt apply, 1 is update fails rule
check_rule :: [Int] -> [Int] -> Int
check_rule update rule = do
    -- undefined update rule
    let lower = head rule
    let higher = head (drop 1 rule)
    test update lower higher

-- 0 if good or doesnt apply, 1 is update fails rule
test :: [Int] -> Int -> Int -> Int
test update lower higher =
    case update of
        i:is -> do
            if i == lower
            then 0
            else do
                if i == higher
                -- then error update
                then do
                    if also_lower is lower
                    then 1
                    else 0
                else test is lower higher
        [] -> 0

also_lower :: [Int] -> Int -> Bool
also_lower is lower = do
    case is of
        [] -> False
        i:is -> do
            if i == lower
            then True
            else also_lower is lower