module Day10 where

import qualified Data.Set as S
import Data.Set (Set)

day10_part1 :: [String] -> IO Int
day10_part1 _xs = do
    let cs = parse _xs
    let cs_set = S.fromList cs
    let trailheads = Prelude.filter (\(Coord _ _ h) -> h == 0) cs
    pure (sum (Prelude.map (\t -> score t cs_set) trailheads))


day10_part2 :: [String] -> IO Int
day10_part2 _xs = do
    let cs = parse _xs
    let cs_set = S.fromList cs
    let trailheads = filter (\(Coord _ _ h) -> h == 0) cs
    pure (sum (map (\t -> score2 t cs_set) trailheads))

data Coord = Coord {x :: Int, y :: Int, h :: Int}
    deriving (Eq, Ord, Show)

score2 :: Coord -> Set Coord -> Int
score2 t cs = length (find_reachable [t] cs)

score :: Coord -> S.Set Coord -> Int
score t cs = length (S.fromList (find_reachable [t] cs))

find_reachable :: [Coord] -> S.Set Coord -> [Coord]
find_reachable currently_at cs = do
    let new = concat (Prelude.map (\f -> get_next f cs) currently_at)
    if length new == 0
    then currently_at
    else find_reachable new cs

get_next :: Coord -> S.Set Coord -> [Coord]
get_next (Coord x y h) cs = filter (flip S.member cs) [(Coord (x+1) y (h+1)), (Coord (x-1) y (h+1)), (Coord x (y+1) (h+1)), (Coord x (y-1) (h+1))]


parse :: [String] -> [Coord]
parse xs = loop_outer 1 xs []

-- xx = (.)

loop_outer :: Int -> [String] -> [Coord] -> [Coord]
loop_outer y ss oldC = do
    case ss of
        s:ss -> do
            let newC = loop_inner 1 y s oldC
            loop_outer (y+1) ss newC
        [] -> oldC

loop_inner :: Int -> Int -> String -> [Coord] -> [Coord]
loop_inner x y ss oldC = do
    case ss of
        s:ss -> loop_inner (x+1) y ss (Coord x y (read [s]):oldC)
        [] -> oldC
