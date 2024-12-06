module Day06 where

import Data.Map.Strict as M
import Prelude hiding (Right, Left)
import Data.Set as S


day6_part1 :: [String] -> IO Int
day6_part1 _xs = do
    let (room, start_c) = construct_room _xs
    -- print room
    -- print start_c
    let visited = walk start_c Up room (S.fromList [start_c])
    -- print visited
    -- print (length visited)
    pure (length visited)

day6_part2 :: [String] -> IO Int
day6_part2 _xs = do
    let (room, start_c) = construct_room _xs
    let potential_blocks = S.toList (S.delete start_c (walk start_c Up room (S.fromList [start_c])))
    let loops = Prelude.filter (\n -> n == True) (Prelude.map (\n -> test_block n start_c Up room) potential_blocks)
    -- print loops
    -- print (length loops)
    pure (length loops)


data Coord = Coord {xc :: Int, yc :: Int}
    deriving (Eq, Ord, Show)

data Direction = Left | Up | Down | Right
    deriving (Eq, Ord, Show)

data CoordD = CoordD {c :: Coord , d :: Direction}
    deriving (Eq, Ord, Show)


test_block :: Coord -> Coord -> Direction -> M.Map Coord String -> Bool
test_block block_c gc gd room = do
    let blocked_room = M.insert block_c "#" room
    walk_loops gc gd blocked_room (S.fromList [])


-- only need to check adding a block to positions in visited from part 1 (not the start) because only places guard stepped to could have blocked the guard

-- need to store visited positions with the direcvtion as well
-- need a different walk, which check length of set before and afetr adding to it. Takes a starting set of (S.Set Coord Direction) and returns a bool, true is blocking

walk_loops :: Coord -> Direction -> M.Map Coord String -> (S.Set CoordD) -> Bool
walk_loops gc gd room visited = do
    let attempted_c = next_position gc gd
    if M.notMember attempted_c room
    then False --walks off edge so doesnt loop
    else do
        if blocked attempted_c room
        then walk_loops gc (rotate gd) room visited
        else do
            let orig_len = length visited
            let new_visited = S.insert (CoordD attempted_c gd) visited
            if orig_len == length new_visited
            then True -- pos and direction not new, so looping
            else walk_loops attempted_c gd room new_visited -- not new so keep going with increased set

 
walk :: Coord -> Direction -> M.Map Coord String -> S.Set Coord -> S.Set Coord
walk gc gd room visited = do
    let attempted_c = next_position gc gd
    if M.notMember attempted_c room
    then visited
    else do
        if blocked attempted_c room
        then walk gc (rotate gd) room visited
        else walk attempted_c gd room (S.insert attempted_c visited)


next_position :: Coord -> Direction -> Coord
next_position (Coord xc yc) d = do
    case d of
        Up -> Coord xc (yc - 1)
        Left -> Coord (xc - 1) yc
        Down -> Coord xc (yc + 1)
        Right -> Coord (xc + 1) yc


blocked :: Coord -> M.Map Coord String -> Bool
blocked c room = do
    let v = M.lookup c room
    case v of
        Just v -> v == "#"
        Nothing -> error "have already check that key is in map"

rotate :: Direction -> Direction
rotate d = do
    case d of
        Up -> Right
        Left -> Up
        Down -> Left
        Right -> Down











construct_room :: [String] -> (M.Map Coord String, Coord)
construct_room xs = loop_outer 1 xs (M.fromList []) (Coord 0 0)


loop_outer :: Int -> [String] -> M.Map Coord String -> Coord -> (M.Map Coord String, Coord)
loop_outer y ss oldM start = do
    case ss of
        s:ss -> do
            let (newM, new_start) = loop_inner 1 y s oldM start
            loop_outer (y+1) ss newM new_start
        [] -> (oldM, start)

loop_inner :: Int -> Int -> String -> M.Map Coord String -> Coord -> (M.Map Coord String, Coord)
loop_inner x y ss oldM start = do
    case ss of
        s:ss -> do
            if s == '^'
            then do
                let newM = M.insert (Coord x y) "." oldM
                loop_inner (x+1) y ss newM (Coord x y)
            else do
                let newM = M.insert (Coord x y) [s] oldM
                loop_inner (x+1) y ss newM start
        [] -> (oldM, start)


