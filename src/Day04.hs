module Day04 where

import Data.Map.Strict as M


day4_part1 :: [String] -> IO Int
day4_part1 _xs = do
    let (puzzle_map, coords_list) = construct_puzzle _xs
    let res = Prelude.map (\c -> get_coords c puzzle_map "XMAS") coords_list
    pure (sum (Prelude.map (\(i, _cl) -> i) (res)))

day4_part2 :: [String] -> IO Int
day4_part2 _xs = do
    let (puzzle_map, coords_list) = construct_puzzle _xs
    let res = Prelude.map (\c -> get_coords2 c puzzle_map "MAS") coords_list
    let not_empty = Prelude.filter (\(i, _css) -> i > 0) res 
    let mas_l = concat (Prelude.map (\(_i, css) -> css) not_empty)
    pure (sum (Prelude.map (\c1 -> sum (Prelude.map (\c2 -> make_x c1 c2) mas_l)) mas_l) `div` 2)
    -- print mas_l
    -- print (length mas_l)
    -- pure (sum (Prelude.map (\(i, _cl) -> i) (res)))

data Coord = Coord {xc :: Int, yc :: Int}
    deriving (Eq, Ord, Show)

make_x :: [Coord] -> [Coord] -> Int
make_x xs ys = do
    if xs == ys
    then 0
    else do
        if head (Prelude.drop 1 xs) == head (Prelude.drop 1 ys)
        then 1
        else 0
    

-- returns int of how many matching at that start coord (0 min 8 max) and list of coords for each answer
get_coords2 :: Coord -> M.Map Coord String -> String-> (Int, [[Coord]])
get_coords2 c m target_s = do
    let cdl = check_down_left m c target_s
    let cdr = check_down_right m c target_s
    let cul = check_up_left m c target_s
    let cur = check_up_right m c target_s
    let all :: [[Coord]] = Prelude.filter (\x -> length x > 0) (cul:cur:cdl:cdr:[])
    (length all, all)

-- returns int of how many matching at that start coord (0 min 8 max) and list of coords for each answer
get_coords :: Coord -> M.Map Coord String -> String-> (Int, [[Coord]])
get_coords c m target_s = do
    let cr :: [Coord] = check_right m c target_s
    let cl = check_left m c target_s
    let cu = check_up m c target_s
    let cd = check_down m c target_s
    let cdl = check_down_left m c target_s
    let cdr = check_down_right m c target_s
    let cul = check_up_left m c target_s
    let cur = check_up_right m c target_s
    let all :: [[Coord]] = Prelude.filter (\x -> length x > 0) (cr:cl:cu:cd:cul:cur:cdl:cdr:[])
    (length all, all)

test_coords_match_string :: Coord -> M.Map Coord String -> String -> Int -> Int -> [Coord]
test_coords_match_string c m target xdiff ydiff = do
    let to_check = coords_to_check c xdiff ydiff (length target) []
    let found = concat (Prelude.map (\c -> case M.lookup c m of Just v -> v; Nothing -> "#") to_check)
    if found == target
    then to_check
    else []

coords_to_check :: Coord -> Int -> Int -> Int -> [Coord] -> [Coord]
coords_to_check (Coord x y) x_diff y_diff length acc = do
    -- undefined x y x_diff y_diff length acc
    if length > 0
    then coords_to_check (Coord (x+x_diff) (y+y_diff)) x_diff y_diff (length - 1) ((Coord x y):acc)
    else reverse acc

check_right :: M.Map Coord String -> Coord -> String -> [Coord]
check_right m c target = test_coords_match_string c m target 1 0

check_left :: M.Map Coord String -> Coord -> String -> [Coord]
check_left m c target = test_coords_match_string c m target (-1) 0

check_up :: M.Map Coord String -> Coord -> String -> [Coord]
check_up m c target = test_coords_match_string c m target (0) (-1)

check_down :: M.Map Coord String -> Coord -> String -> [Coord]
check_down m c target = test_coords_match_string c m target (0) (1) 

check_down_right :: M.Map Coord String -> Coord -> String -> [Coord]
check_down_right m c target = test_coords_match_string c m target (1) (1)

check_down_left :: M.Map Coord String -> Coord -> String -> [Coord]
check_down_left m c target = test_coords_match_string c m target (-1) (1)

check_up_right :: M.Map Coord String -> Coord -> String -> [Coord]
check_up_right m c target = test_coords_match_string c m target (1) (-1)

check_up_left :: M.Map Coord String -> Coord -> String -> [Coord]
check_up_left m c target = test_coords_match_string c m target (-1) (-1)


construct_puzzle :: [String] -> (M.Map Coord String, [Coord])
construct_puzzle xs = loop_outer 1 xs (M.fromList []) []

loop_outer :: Int -> [String] -> M.Map Coord String -> [Coord] -> (M.Map Coord String, [Coord])
loop_outer y ss oldM oldL = do
    case ss of
        s:ss -> do
            let (newM, newL) = loop_inner 1 y s oldM oldL
            loop_outer (y+1) ss newM newL
        [] -> (oldM, oldL)

loop_inner :: Int -> Int -> String -> M.Map Coord String -> [Coord] -> (M.Map Coord String, [Coord])
loop_inner x y ss oldM oldL= do
    case ss of
        s:ss -> do
            let newM = M.insert (Coord x y) [s] oldM
            let newL = (Coord x y):oldL
            loop_inner (x+1) y ss newM newL
        [] -> (oldM, oldL)