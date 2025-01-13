module Day18 where

import Data.List.Split

import qualified Data.Set as S
import Data.Set (Set)

day18_part1 :: [String] -> Int -> Int -> IO Int
day18_part1 _xs size num_fallen = do
    let all_blocks = map parse _xs
    let fallen = S.fromList (take num_fallen all_blocks)
    let all_coords = S.fromList [ Coord x y  | x <- [0..size],  y <- [0..size]]
    let maybe_num_steps = steps [Coord 0 0] (S.fromList []) 0 fallen all_coords (Coord size size)
    case maybe_num_steps of
        Just num -> pure num
        Nothing -> error "part 1 should not be completely blocked"


day18_part2 :: [String] -> Int -> IO String
day18_part2 _xs size = do
    let all_blocks = map parse _xs
    let all_coords = S.fromList [ Coord x y  | x <- [0..size],  y <- [0..size]]
    try_all (reverse all_blocks) all_coords (Coord size size)


data Coord = Coord {x :: Int, y :: Int}
    deriving (Eq, Ord, Show)

-- take reverse of blocks, so can keep removing the head and recurese trying with one les block
-- this will never rtry the last block?? maybe?
try_all :: [Coord] -> Set Coord -> Coord -> IO String
try_all reverse_blocks all target = do
    case reverse_blocks of
        [] -> error "should nto run out of blocks to try before finding a path"
        ((Coord x y):rest) -> do
            let maybe_num_steps = steps [Coord 0 0] (S.fromList []) 0 (S.fromList rest) all target
            case maybe_num_steps of
                Just _num -> pure ((show(x)) ++ "," ++ (show(y)))
                Nothing -> try_all rest all target


steps :: [Coord] -> Set Coord -> Int -> Set Coord -> Set Coord -> Coord -> Maybe Int
steps current visited count blocks all target = do
    let next_current_set :: Set Coord = S.fromList (concat (map (\c -> get_next c blocks all (S.union (S.fromList current) visited)) current))
    let new_count = count + 1
    if S.member target next_current_set
    then Just new_count
    else do
        let next_current = S.toList next_current_set
        if length next_current == 0
        then Nothing
        else do
            let new_visited = S.union visited (S.fromList current)
            steps next_current new_visited new_count blocks all target


get_next :: Coord -> Set Coord -> Set Coord -> Set Coord -> [Coord]
get_next (Coord x y) blocks all visited = do
    let potentials = [(Coord (x+1) y), (Coord x (y+1)), (Coord (x-1) y), (Coord x (y-1))]
    filter (\c -> S.notMember c visited) (filter (\c -> S.notMember c blocks) (filter (\c -> S.member c all) potentials))


parse :: String -> Coord
parse xs = do
    let is = map read (splitOn "," xs)
    Coord (head is) (head (reverse is))