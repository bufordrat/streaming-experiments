module SimpleList where

import Data.Char (toUpper)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n lst
  | length lst <= n = [lst]
  | otherwise = take n lst : chunksOf n (drop n lst)

processData :: String -> [String]
processData input =
  map (map toUpper . reverse) $ lines input
