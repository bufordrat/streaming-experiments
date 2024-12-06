module NotFromScratch where

import Streaming as S
import qualified Streaming.Prelude as S

withTab :: Int -> String
withTab num = show num <> "\t"

tabulate :: Int ->
            Stream (Of Int) IO r ->
            Stream (Of String) IO r
tabulate cols strm = S.mapsM S.mconcat
                     . S.chunksOf cols
                     . S.map withTab
                     $ strm

listToStream :: Monad m => [a] -> Stream (Of a) m ()
listToStream = S.each

sumAndTab :: Int -> Stream (Of Int) IO r -> IO Int
sumAndTab cols strm = fmap S.fst'
                      . S.mapM_ putStrLn
                      . tabulate cols
                      . S.store S.sum
                      $ strm

strm :: Stream (Of Int) IO ()
strm = listToStream [1..9]
