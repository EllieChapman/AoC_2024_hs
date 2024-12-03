module Day03part2 where

import Data.Char


day3_part2 :: [String] -> Int
day3_part2 _xs = do
    wrap _xs 0 Do

wrap :: [String] -> Int -> Mode -> Int
wrap xs acc m = do
    case xs of
        x:xs -> do
            let (res, newm) = find_muls x 0 m
            wrap xs (res+acc) newm
        [] -> acc


data Mode = Do | Dont | Unchanged

find_muls :: String -> Int -> Mode -> (Int, Mode)
find_muls xs res orig_m = do
    let (may_i, may_s, new_m) = find_next_mul xs orig_m
    case may_s of
        Just s -> do
            case may_i of
                Just i -> find_muls s (res+i) new_m
                Nothing -> find_muls s res new_m
        Nothing -> do
            case may_i of
                Just i -> (res+i, new_m)
                Nothing -> (res, new_m)

find_next_mul :: String -> Mode -> (Maybe Int, Maybe String, Mode)
find_next_mul xs origM = do
    --scan until find mul, keep going until have it. then locked into place while test next characters
    let (maybe_mul, maybe_newM) = get_mul xs origM
    -- ebc here need to handle, what if mul returned having dfound do or dont
    case maybe_newM of
        Do -> do
            case maybe_mul of
                Just s -> (Nothing, Just s, Do)
                Nothing -> error "stirng should not be nothing when found Do"
        Dont -> do
            case maybe_mul of
                Just s -> (Nothing, Just s, Dont)
                Nothing -> error "stirng should not be nothing when found Dont"
        Unchanged -> do
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
                                    then (Just (i1*i2), Just s5, origM)
                                    else (Nothing, Just s5, origM)
                                else (Nothing, Just s4, origM)
                            else (Nothing, Just s3, origM)
                        else (Nothing, Just s2, origM)
                    else (Nothing, Just s1, origM)
                Nothing -> (Nothing, Nothing, origM) -- no "mul" left in string, so no int or remaining string to return

-- keeps going until can match on mul, then returns remaining string after. if no string no mul is left
get_mul :: String -> Mode -> (Maybe String, Mode)

-- these four if nto found return original string, if found return string after whateveer is parsed out
get_left :: String -> (Bool, String)
get_right :: String -> (Bool, String)
get_comma :: String -> (Bool, String)
-- this needs at least one int digit, but needs to keep going until not an int. returns int if can find one and remaining string if so. if cant find string then start again with whatever was passed in
get_int :: String -> (Bool, Int, String)


match_do :: String -> (Bool, String)
match_do xs = do
    case xs of
        a:b:c:d:xs2 -> do
            if [a] ++ [b] ++ [c] ++ [d] == "do()"
            then (True, xs2)
            else (False, [])
        _ -> (False, [])

match_dont :: String -> (Bool, String)
match_dont xs = do
    case xs of
        a:b:c:d:e:f:g:xs2 -> do
            if [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] == "don't()"
            then (True, xs2)
            else (False, [])
        _ -> (False, [])

match_mul :: String -> (Bool, String)
match_mul xs = do
    case xs of
        x:y:z:xs2 -> do
            if [x] ++ [y] ++ [z] == "mul"
            then (True, xs2)
            else (False, [])
        _ -> (False, [])


-- EBC also check if find do or don't, then return with string after and new mode (which might be the same as old mode)
-- need to check current mode, and only try to match on mul if in do currently
get_mul xs m = do
    case m of
        Do -> do
            let (b, s) = match_dont xs
            if b
            then (Just s, Dont)
            else do
                let (b2, s2) = match_do xs
                if b2
                then (Just s2, Do)
                else do
                    let (b3, s3) = match_mul xs
                    if b3
                    then (Just s3, Unchanged)
                    else do
                        case xs of
                            _x:xs -> get_mul xs Do
                            [] -> (Nothing, Unchanged)
        Dont -> do
            let (b, s) = match_dont xs
            if b
            then (Just s, Dont)
            else do
                let (b2, s2) = match_do xs
                if b2
                then (Just s2, Do)
                else do
                    case xs of
                        _x:xs -> get_mul xs Dont
                        [] -> (Nothing, Unchanged)
        Unchanged -> error "shouldnt be unchanged ehre"


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
