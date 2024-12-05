module Day05 where
import Data.List.Split

-- part1 :: Input -> Int
-- part1 (cs,us) = sum [ middle u | u <- us, u `satisfies` cs ]
--   sum (map middle (filter (\u -> u `satisfies` cs) us))

day5_part1 :: [String] -> Int
day5_part1 _xs = do
    let rules :: [[Int]] = map (\n -> map read (splitOn "|" n)) (get_rule_lines _xs [])
    let updates :: [[Int]] = map (\n -> map read (splitOn "," n)) (drop ((length rules) + 1) _xs)
    sum (map (\n -> test_update n rules) updates)

day5_part2 :: [String] -> IO Int
day5_part2 _xs = do
    let rules :: [[Int]] = map (\n -> map read (splitOn "|" n)) (get_rule_lines _xs [])
    let updates :: [[Int]] = map (\n -> map read (splitOn "," n)) (drop ((length rules) + 1) _xs)
    let bad_updates :: [[Int]] = filter (\u -> test_update u rules == 0) updates
    let fixed_bad :: [[Int]] = map (\n -> fix_bad n rules) bad_updates
    pure (sum (map middle fixed_bad))

get_rule_lines :: [String] -> [String] -> [String]
get_rule_lines to_check found = do
    case to_check of
        x:xs -> do
            case x of
                _:_ -> get_rule_lines xs (x:found)
                [] -> found
        [] -> error "this should not reach the end of input lines"

fix_bad :: [Int] -> [[Int]] -> [Int]
fix_bad bad rules = do
    fix_pairs bad rules 1 2


fix_pairs :: [Int] -> [[Int]] -> Int -> Int -> [Int]
fix_pairs update rules first second = do
    if first >= length update
    then update
    else do
        if second > length update
        then fix_pairs update rules (first + 1) (first + 1)
        else do
            let (first_i, second_i) = get_pair update first second
            let res = test_update [first_i, second_i] rules
            if res > 0 --true when pair is in correct order
            then fix_pairs update rules first (second + 1) --keep going and check next pair
            else do
                let new_update = swap update first second
                fix_pairs new_update rules 1 2 --back to start, but update is slightly better
                

get_pair :: [Int] -> Int -> Int -> (Int, Int)
get_pair update pos1 pos2 = (head (drop (pos1 - 1) update), head (drop (pos2 - 1) update))

swap :: [Int] -> Int -> Int -> [Int]
swap update pos1 pos2 = do
    let (i1, i2) = get_pair update pos1 pos2
    let u2 = map (\i -> if i == i1; then 0; else i) update
    let u3 = map (\i -> if i == i2; then i1; else i) u2
    let u4 = map (\i -> if i == 0; then i2; else i) u3
    u4


-- either returns middle page num or 0
test_update :: [Int] -> [[Int]] -> Int
test_update update rules = do
    let score = sum (map (\n -> check_rule update n) rules)
    if score == 0
    then middle update
    else 0

middle :: [Int] -> Int
middle is = do
    head (drop ((length is) `div` 2) is)

-- 0 if good or doesnt apply, 1 is update fails rule
check_rule :: [Int] -> [Int] -> Int
check_rule update rule = do
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