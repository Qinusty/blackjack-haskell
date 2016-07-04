module Utils where


-- | Takes two lists, first is the query, second is the search list
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = False
startsWith _ [] = False
startsWith (c:[]) (c':_) = c == c'
startsWith (c:cs) (c':cs') = c == c' && startsWith cs cs'
