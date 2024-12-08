module Day08 where
import Data.Set as S

day8_part1 :: [String] -> IO Int
day8_part1 _xs = do
    let (ants, all_coords) :: ([Ant], S.Set Coord) = parse _xs
    pure (length (S.intersection (S.fromList (concat (Prelude.map test_ant ants))) all_coords))

day8_part2 :: [String] -> IO Int
day8_part2 _xs = do
    let (ants, all_coords) :: ([Ant], S.Set Coord) = parse _xs
    pure (length (S.fromList (concat (Prelude.map (\a -> test_ant2 a all_coords) ants))))

data Coord = Coord {xc :: Int, yc :: Int}
    deriving (Eq, Ord, Show)

data Ant = Ant String [Coord]
    deriving (Eq, Ord, Show)


test_ant2 :: Ant -> S.Set Coord -> [Coord]
test_ant2 (Ant _s cs) c_set = concat (Prelude.map (\c -> test_coord2 c cs c_set) cs)

test_coord2 :: Coord -> [Coord] -> S.Set Coord -> [Coord]
test_coord2 c1 cs c_set = concat (Prelude.map (\c2 -> test_pair2 c1 c2 c_set) cs)

test_pair2 :: Coord -> Coord -> S.Set Coord -> [Coord]
test_pair2 (Coord x1 y1) (Coord x2 y2) c_set = do
    let xdiff = x1 - x2
    let ydiff = y1 - y2
    if xdiff == 0 && ydiff == 0
    then []
    else (get_plus x1 y1 xdiff ydiff c_set []) ++ (get_plus x2 y2 (-xdiff) (-ydiff) c_set [])


get_plus :: Int -> Int -> Int -> Int -> S.Set Coord -> [Coord] -> [Coord]
get_plus x y xdiff ydiff cs acc = do
    if S.member (Coord x y) cs
    then get_plus (x+xdiff) (y+ydiff) xdiff ydiff cs ((Coord x y):acc)
    else acc



test_ant :: Ant -> [Coord]
test_ant (Ant _s cs) = concat (Prelude.map (\c -> test_coord c cs) cs)

test_coord :: Coord -> [Coord] -> [Coord]
test_coord c1 cs = concat (Prelude.map (\c2 -> test_pair c1 c2) cs)

test_pair :: Coord -> Coord -> [Coord]
test_pair (Coord x1 y1) (Coord x2 y2) = do
    let xdiff = x1 - x2
    let ydiff = y1 - y2
    if xdiff == 0 && ydiff == 0
    then []
    else [Coord (x1 + xdiff) (y1 + ydiff), Coord (x2 - xdiff) (y2 - ydiff)]


parse :: [String] -> ([Ant], S.Set Coord)
parse xs = loop_outer 1 xs [] (S.fromList [])

loop_outer :: Int -> [String] -> [Ant] -> S.Set Coord -> ([Ant], S.Set Coord)
loop_outer y ss oldA oldC = do
    case ss of
        s:ss -> do
            let (newA, newC) = loop_inner 1 y s oldA oldC
            loop_outer (y+1) ss newA newC
        [] -> (oldA, oldC)

loop_inner :: Int -> Int -> String -> [Ant] -> S.Set Coord -> ([Ant], S.Set Coord)
loop_inner x y ss oldA oldC = do
    case ss of
        s:ss -> do
            if [s] == "."
            then loop_inner (x+1) y ss oldA (S.insert (Coord x y) oldC)
            else
                if is_new oldA [s]
                then do
                    let newA = (Ant [s] [(Coord x y)]):oldA
                    loop_inner (x+1) y ss newA (S.insert (Coord x y) oldC)
                else do
                    let newA = Prelude.map (\a -> merge_ant a [s] (Coord x y)) oldA
                    loop_inner (x+1) y ss newA (S.insert (Coord x y) oldC)
        [] -> (oldA, oldC)

merge_ant :: Ant -> String -> Coord -> Ant
merge_ant (Ant s cs) new_s c = do
    if s == new_s
    then Ant s (c:cs)
    else Ant s cs

is_new :: [Ant] -> String -> Bool
is_new as s1 = do
    let found = Prelude.filter (\(Ant s2 _) -> s1 == s2) as
    if length found == 0
    then True
    else False