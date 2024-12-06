module TooManyPasses where

import Data.Foldable (traverse_)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n lst
  | length lst <= n = [lst]
  | otherwise = take n lst : chunksOf n (drop n lst)

withTab :: Int -> String
withTab num = show num <> "\t"

tabulate :: Int -> [Int] -> [String]
tabulate cols ns = map mconcat
                   . chunksOf cols
                   . map withTab
                   $ ns

sumAndTab :: Int -> [Int] -> IO Int
sumAndTab cols ns = do
  traverse_ putStrLn (tabulate cols ns)
  pure (sum ns)

-- λ> sumAndTab 3 [1..9]
