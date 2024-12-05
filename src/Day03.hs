module Day03 where

import Data.Char

day3_part1 :: [String] -> Int
day3_part1 _xs = do
    -- Make one long string. Add 'A' between lines in place of newline character
    let input = concat (map (\n -> n ++ "A") _xs)
    let tokens = get_token_list input []
    sum (map interpret_muls tokens)


day3_part2 :: [String] -> Int
day3_part2 _xs = do
    let input = concat (map (\n -> n ++ "A") _xs)
    let tokens = reverse(get_token_list input [])
    interpret_mode_muls tokens 0 True


data Token = Mul Int Int Int | Do | Dont | Rubbish String -- rubbish always just a single character string

interpret_muls :: Token -> Int
interpret_muls t = do
    case t of
        Mul _ _ prod -> prod
        _ -> 0

-- list yet to parse, accumulated total, mode where true = do, returns total
interpret_mode_muls :: [Token] -> Int -> Bool -> Int
interpret_mode_muls ts acc b = do
    case ts of
        t:ts -> do
            case t of
                Do -> interpret_mode_muls ts acc True
                Dont -> interpret_mode_muls ts acc False
                Rubbish _ -> interpret_mode_muls ts acc b
                Mul _ _ prod -> do
                    if b
                    then interpret_mode_muls ts (acc + prod) b
                    else interpret_mode_muls ts acc b
        [] -> acc

get_token_list :: String -> [Token] -> [Token]
get_token_list xs tokens_found = do
    case xs of
        _:_ -> do
            let (t, s) = get_next_token xs
            get_token_list s (t:tokens_found)
        [] -> tokens_found

get_next_token :: String -> (Token, String)
get_next_token xs = do
    case xs of
        _:_ -> do
            let (maybe_t, s) = get_do xs
            case maybe_t of
                Just t -> (t, s)
                Nothing -> do
                    let (maybe_t, s) = get_dont xs
                    case maybe_t of
                        Just t -> (t, s)
                        Nothing -> do
                            let (maybe_t, s) = get_mul xs
                            case maybe_t of
                                Just t -> (t, s)
                                Nothing -> do
                                    let (maybe_t, s) = get_rubbish xs
                                    case maybe_t of
                                        Just t -> (t, s)
                                        Nothing -> error "should match on one type of Token"
        [] -> error "should have already tested for empty string"


get_do :: String -> (Maybe Token, String)
get_dont :: String -> (Maybe Token, String)
get_mul :: String -> (Maybe Token, String)
get_rubbish :: String -> (Maybe Token, String)

get_do xs = do
    case xs of
        a:b:c:d:xs2 -> do
            if [a] ++ [b] ++ [c] ++ [d] == "do()"
            then (Just Do, xs2)
            else (Nothing, [])
        _ -> (Nothing, [])

get_dont xs = do
    case xs of
        a:b:c:d:e:f:g:xs2 -> do
            if [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] == "don't()"
            then (Just Dont, xs2)
            else (Nothing, [])
        _ -> (Nothing, [])
    
get_rubbish xs = do
    case xs of
        x:xs2 -> (Just (Rubbish [x]), xs2)
        _ -> error "should be at least one char here"
    
get_mul xs = do
    case xs of
        a:b:c:d:xs2 -> do
            if [a,b,c,d] == "mul("
            then do
                let (b1, i1, xs3) = get_int xs2
                if b1
                then do
                    let (b2, xs4) = get_comma xs3
                    if b2
                    then do
                        let (b3, i2, xs5) = get_int xs4
                        if b3
                        then do
                            let (b4, xs6) = get_right xs5
                            if b4
                            then (Just (Mul i1 i2 (i1*i2)), xs6)
                            else (Nothing, [])
                        else (Nothing, [])
                    else (Nothing, [])
                else (Nothing, [])
            else (Nothing, [])
        _ -> (Nothing, [])


get_right :: String -> (Bool, String)
get_comma :: String -> (Bool, String)
get_int :: String -> (Bool, Int, String)

get_right xs = do
    case xs of
        x:xs -> do
            if x == ')'
            then (True, xs)
            else (False, x:xs)
        [] -> (False, [])

get_comma xs = do
    case xs of
        x:xs -> do
            if x == ','
            then (True, xs)
            else (False, x:xs)
        [] -> (False, [])

get_int xs = do
    case xs of
        x:xs -> do
            if isDigit x
            then do
                let (int_s, remaining) = get_i xs [x]
                (True, read int_s, remaining)
            else (False, 0, x:xs)
        [] -> (False, 0, [])


get_i :: String -> String -> (String, String)
get_i to_check int_string = do
    case to_check of
        x:xs -> do
            if isDigit x
            then get_i xs (int_string ++ [x])
            else (int_string, x:xs)
        [] -> (int_string, [])
