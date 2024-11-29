module FromScratch where

data Stream e m r
  = Element e (Stream e m r)
  | Action (m (Stream e m r))
  | Result r

empty :: Stream e m ()
empty = Result ()

exampleStream :: Stream Int IO ()
exampleStream = Element 1
                (Action (putStrLn "some action" >>
                         pure (Element 2
                                   (Action (putStrLn "finish" >>
                                            pure empty)))))

printStream :: (Show e, Show r) =>
               Stream e IO r -> IO ()
printStream (Result r) = putStrLn $ "Result: " <> show r
printStream (Element e str) = do
  putStrLn $ "Element: " <> show e
  printStream str
printStream (Action mstr) = do
  putStr "Run action: "
  str <- mstr
  printStream str

ssum :: (Num e, Monad m) => Stream e m r -> m (e, r)
ssum (Result r) = pure (0, r)
ssum (Action m) = m >>= ssum
ssum (Element e str) =
  let eachElem (acc, r) = (acc + e, r)
  in eachElem <$> ssum str

listToStream :: [e] -> Stream e m ()
listToStream [] = Result ()
listToStream (x : xs) = Element x (listToStream xs)
