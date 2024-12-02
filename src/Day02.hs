module Day02 where

day2_part1 :: [String] -> Int
day2_part1 _xs = do
    let levels = map (\n -> map read (words n)) _xs
    let ordered_levels = map order_increasing levels
    let scores = map test_level ordered_levels
    sum scores


day2_part2 :: [String] -> Int
day2_part2 _xs = do
    let levels = map (\n -> map read (words n)) _xs
    let scores = map test_level_dampened levels -- don't reverse yet, as might need to change if 1st or 2nd element if removed 
    sum scores

-- check increasing list
-- return 1 if safe, 0 is bad
test_level :: [Int] -> Int
test_level is = do
    case is of
        i1:is -> do
            case is of
                i2:_ -> do
                    let diff = i2 - i1
                    if (0 < diff && diff < 4)
                    then test_level is
                    else 0
                [] -> 1 --only 1 thing left in list, checked already so good
        [] -> error "should not reach empty, or start empty"


order_increasing :: [Int] -> [Int]
order_increasing i1s = do
    case i1s of
        i1:i2s -> do
            case i2s of
                i2:_ -> do
                    if i1 > i2
                    then reverse i1s
                    else i1s
                [] -> error "can't be one element long"
        [] -> error "can't be empty to start"

-- need to do orderign here, geenrate a;ll possibiities
test_level_dampened :: [Int] -> Int
test_level_dampened is = do
    let both_orders = (remove_item [] is []) ++ (remove_item [] (reverse is) []) --generate all possible lists with an item removed, in both orders
    let results = map test_level both_orders
    if sum results > 0
    then 1
    else 0


remove_item :: [Int] -> [Int] -> [[Int]] -> [[Int]]
remove_item first second res = do
    case second of
        x:xs -> remove_item (first ++ [x]) xs (res ++ [first ++ xs])
        [] -> res


