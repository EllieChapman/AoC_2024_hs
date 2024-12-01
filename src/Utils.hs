module Utils (readLinesFromFile, check) where
    
readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile p =  do
    s <- readFile p
    pure (lines s)


check :: (Eq a, Show a) => a -> a -> a
check a b = if a == b then a else error ("check failed: " ++ show a ++ " not same as: " ++ show b)