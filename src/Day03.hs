module Day03 where

import Data.Char

day3_part1 :: [String] -> Int
day3_part1 _xs = sum (map (\n -> find_muls n 0) _xs)

find_muls :: String -> Int -> Int
find_muls xs res = do
    let (may_i, may_s) = find_next_mul xs
    case may_s of
        Just s -> do
            case may_i of
                Just i -> find_muls s (res+i)
                Nothing -> find_muls s res
        Nothing -> do
            case may_i of
                Just i -> res+i
                Nothing -> res

find_next_mul :: String -> (Maybe Int, Maybe String)
find_next_mul xs = do
    --scan until find mul, keep going until have it. then locked into place while test next characters
    let maybe_mul = get_mul xs
    case maybe_mul of
        Just new_xs -> do -- found next "mul" and have string immediately after to check.
            let (b1, s1) = get_left new_xs
            if b1
            then do
                let (b2, i1, s2) = get_int s1
                if b2
                then do
                    let (b3, s3) = get_comma s2
                    if b3
                    then do
                        let (b4, i2, s4) = get_int s3
                        if b4
                        then do
                            let (b5, s5) = get_right s4
                            if b5
                            then (Just (i1*i2), Just s5)
                            else (Nothing, Just s5)
                        else (Nothing, Just s4)
                    else (Nothing, Just s3)
                else (Nothing, Just s2)
            else (Nothing, Just s1)
        Nothing -> (Nothing, Nothing) -- no "mul" left in string, so no int or remaining string to return


-- keeps going until can match on mul, then returns remaining string after. if no string no mul is left
get_mul :: String -> Maybe String

-- these four if nto found return original string, if found return string after whateveer is parsed out
get_left :: String -> (Bool, String)
get_right :: String -> (Bool, String)
get_comma :: String -> (Bool, String)
-- this needs at least one int digit, but needs to keep going until not an int. returns int if can find one and remaining string if so. if cant find string then start again with whatever was passed in
get_int :: String -> (Bool, Int, String)


get_mul xs = do
    case xs of
        x:y:z:xs -> do
            if [x] ++ [y] ++ [z] == "mul"
            then Just xs
            else get_mul ([y] ++ [z] ++ xs)
        _x:_xs -> Nothing --end of string so no "mul"
        [] -> Nothing --end of string so no "mul"

get_left xs = do
    case xs of
        x:xs -> do
            if x == '('
            then (True, xs)
            else (False, x:xs)
        [] -> (False, [])

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
