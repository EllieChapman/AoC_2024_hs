module Day19 where

import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)


day19_part1 :: [String] -> IO Int
day19_part1 _xs = do
    let towels = drop 2 _xs
    -- print towels
    let blocks = splitOn ", " (head _xs)
    -- print blocks
    let possible = filter (\t -> isPossible t blocks) towels
    -- print possible
    pure (length possible)


day19_part2 :: [String] -> IO Int
day19_part2 _xs = do
    let towels = drop 2 _xs
    let blocks = splitOn ", " (head _xs)
    let nums = map (\(i, _s) -> i) (map (\t -> try2 t blocks blocks 0 (M.fromList [])) towels)
    -- print nums
    pure (sum nums)


isPossible :: String -> [String] -> Bool
isPossible t bs = try t bs bs


try :: String -> [String] -> [String] -> Bool
try t remaining_blocks all_blocks = do
    if length t == 0
    then True -- all towel matched so done
    else do
        if length remaining_blocks == 0
        then False --still have towel left to match but no more blocks to try, unwind to previous choice point
        else do
            if match t (head remaining_blocks)
            then do
                if try (drop (length (head remaining_blocks)) t) all_blocks all_blocks
                then True
                else try t (drop 1 remaining_blocks) all_blocks
            else try t (drop 1 remaining_blocks) all_blocks


match :: String -> String -> Bool
match t block = do
    if length t < length block
    then False
    else take (length block) t == block


-- possible optimisation, keep track in set of all remaining towels have checked before.
-- if already seen then must be false, doesnt matter if got here in a new way
try2 :: String -> [String] -> [String] -> Int -> Map String Int -> (Int, Map String Int)
try2 t remaining_blocks all_blocks count os = do
    if length t == 0
    then (count + 1, os) -- all towel matched so found another solution
    else do
        if length remaining_blocks == 0
        then (count, os) --still have towel left to match but no more blocks to try, unwind to previous choice point
        else do
            if match t (head remaining_blocks)
            then do
                let new_t = drop (length (head remaining_blocks)) t
                if M.member new_t os
                then do
                    case M.lookup new_t os of
                        Nothing -> error "should always be here if seen before"
                        Just c -> (count + c, os)
                else do
                    let (new_c, os2) = try2 new_t all_blocks all_blocks count os
                    let c_diff = new_c - count
                    let os3 = M.insert new_t c_diff os2
                    try2 t (drop 1 remaining_blocks) all_blocks new_c os3
            else try2 t (drop 1 remaining_blocks) all_blocks count os


-- problem, now 6 again, because corerectly is not gong down every path so wont get all possibilities.
-- need to instead map state to how many extra counts we get from going down that path
-- that way don't need to explore multiple times just add correct amount to count if see already observed state again

-- 2252234696 too low