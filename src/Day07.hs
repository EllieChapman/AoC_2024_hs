module Day07 where
import Data.List.Split
-- import Data.NumberLength.Int
-- import GHC.Integer.Logarithms ( integerLogBase# )

day7_part1 :: [String] -> IO Int
day7_part1 _xs = do
    let eqs = map parse_e _xs
    let res = map (\(E t is) -> test_e (head is) (E t (drop 1 is)) [Mul, Add] [Mul, Add]) eqs
    pure (sum res)

day7_part2 :: [String] -> IO Int
day7_part2 _xs = do
    let eqs = map parse_e _xs
    let res = map (\(E t is) -> test_e (head is) (E t (drop 1 is)) [Mul, Add, Ccat] [Mul, Add, Ccat]) eqs
    pure (sum res)

data E = E Int [Int]
    deriving (Eq, Ord, Show)

data Op = Mul | Add | Ccat

parse_e :: String -> E
parse_e xs = do
    case xs of
        [] -> error "input string shouldn't be empty"
        _:_ -> do
            let is = map read (splitOn " " (filter (\c -> c /= ':') xs))
            E (head is) (drop 1 is)

-- to start pass in value as first int in list, and e is one int less. ops contains both
--gives back tagrte value or 0
test_e :: Int -> E -> [Op] -> [Op] -> Int
test_e current_val (E target is) ops_to_try all_ops =
    case is of
        [] -> do
            if current_val == target
            then target -- used all ints and current vakue is what need, so found
            else 0 --no more ints to use, haven't found it, so return 0/backtrack. might not fail completely
        i:is -> do
            case ops_to_try of
                [] -> 0 -- tried all ops in current position, so return 0/backtrack. might not fail completely
                o:os -> do
                    let new_val = do_op current_val o i
                    if test_e new_val (E target is) all_ops all_ops == target
                    then target --found a way to make target
                    else test_e current_val (E target (i:is)) os all_ops

do_op :: Int -> Op -> Int -> Int
do_op i1 o i2 =
    case o of
        Mul -> i1 * i2
        Add -> i1 + i2
        Ccat -> do
            let num_digits :: Int = (floor (logBase 10 (fromIntegral i2 :: Float))) + 1
            i1 * (10^num_digits) + i2
