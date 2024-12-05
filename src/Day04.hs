module Day04 where

import Data.Map.Strict as M

-- day4_part1 :: [String] -> M.Map (Int, Int) String
-- day4_part1 _xs = construct_puzzle _xs

day4_part1 :: [String] -> Int
day4_part1 _xs = do
    let (puzzle_map, coords_list) = construct_puzzle _xs
    -- get back [[Coord]], each representing a match on xmas
    let coords = get_coords puzzle_map coords_list "xmas" []
    -- need to check length of those not empty
    length coords

day4_part2 :: [String] -> Int
day4_part2 _xs = 0

data Coord = Coord {xc :: Int, yc :: Int}
    deriving (Eq, Ord)

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


-- each inner list is a list of coords that match target word
get_coords :: M.Map Coord String -> [Coord] -> String -> [[Coord]] -> [[Coord]]
get_coords m cs target_s acc = do
    -- Prelude.map (\n -> concat(check_start m n target_s)) cs
    case cs of
        c:cs -> do
            let found_cs = check_start m c target_s
            case found_cs of
                Just found -> get_coords m cs target_s (acc++found)
                Nothing -> get_coords m cs target_s acc
        [] -> acc


check_start :: M.Map Coord String -> Coord -> String -> Maybe [[Coord]]
check_start m c target = do
    -- undefined m c target
    let cr = check_right m c target
    let cl = check_left m c target
    let all = cr:cl:[]
    let only_full = remove_empty all []
    if length only_full > 0
    then Just only_full
    else Nothing

remove_empty :: [[Coord]] -> [[Coord]] -> [[Coord]]
remove_empty to_check checked = do
    case to_check of
        x:xs -> do
            case x of
                _:_ -> remove_empty xs (x:checked)
                [] -> remove_empty xs checked
        [] -> checked

check_right :: M.Map Coord String -> Coord -> String -> [Coord]
check_right m c s = do
    undefined m c s

check_left :: M.Map Coord String -> Coord -> String -> [Coord]
check_left m c s = do
    undefined m c s

-- check_all_coords :: M.Map Coord String -> Coord -> String -> [[Coord]] -> Int -> Int -> [[Coord]]
-- check_all_coords m coord target_s acc num_y num_y = do
--     case coord of
--     let res = score_coord coord m target_s
--     case res of
--         Just coords -> do
            
--             -- coord:acc
-- --iterate over all coords in map, pass in to score cord. If get Just [Coord] add to accumulated results then recurse. return acc when done
-- -- to loop go until equal to numx and num y
--     undefined m target_s acc

-- score_coord :: Coord -> M.Map Coord String -> String -> Maybe [Coord]
-- score_coord _c _m _target_s =
--     0

-- add_point :: Int -> Int -> String -> M.Map (Int, Int) String -> M.Map (Int, Int) String
-- add_point x y ss oldM = do
--     case ss of
--         s:_ -> M.insert (x, y) [s] oldM
--         [] -> error "should never have no more points"

-- have list where outer index is y, inner is x
-- make map of points to letter

-- iterate over points, if x call check xmas functions

-- check xmas goes thorugh 8 heklper functions. sums results together

-- 8 functions each take starting x coord then look for mas in corrcet directtion. 1 (found) or 0
-- check up
-- up right
-- right
-- down right
-- down
-- down left
-- left
-- up left