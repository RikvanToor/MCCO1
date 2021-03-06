module Utils.Similarity(similar) where


import Data.Char
import Utils.Utils(internalError)


similar :: String -> String -> Bool
-- ignore primitives
similar ('$':c:_) _  | isAlpha c  = False
similar _ ('$':c:_)  | isAlpha c  = False
-- normal test
similar name1' name2' =
    name1 == name2
    ||
    oneDiff name1 name2
    || 
    oneMore name1 name2
    ||
    oneMore name2 name1
    || 
    name1 `elem` swap name2
    where
        name1 = map toUpper name1'
        name2 = map toUpper name2'

oneMore :: String -> String -> Bool
oneMore xs ys = 
    length xs - length ys == 1
    &&
    ys `elem` dropOne xs

dropOne :: String -> [String]
dropOne []     = []
dropOne (x:xs) = xs : map (x:) (dropOne xs)

oneDiff :: String -> String -> Bool
oneDiff xs ys = 
    length xs == length ys
    &&
    length (filter (== True) (zipWith (==) xs ys)) == length xs - 1

swap :: [a] -> [[a]]
swap [_] = []
swap (x:y:xs) = (y:x:xs) : map (x:) (swap (y:xs))
swap [] = internalError "Similarity" "swap" "empty string"
