module NotFromScratch where

import Data.Text (Text, pack)
import Data.Text.IO as TIO
import TextShow
import qualified Data.List.Extra as LE

import Streaming as S
import qualified Streaming.Prelude as S

withTab :: Int -> Text
withTab num = showt num <> (pack "\t")

tabulateL :: Int -> [Int] -> [Text]
tabulateL cols ns = map mconcat $ LE.chunksOf cols $ map withTab ns

sumAndTabL :: Int -> [Int] -> IO Int
sumAndTabL cols ns = do
  mapM_ TIO.putStrLn $ tabulateL cols ns
  pure $ sum ns

