module FromScratchFunc where

import Data.Bifunctor

data Stream f m r
  = Step (f (Stream f m r))
  | Effect (m (Stream f m r))
  | Return r

data Of a b = a :> b
  deriving Show

instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap f (Return r) = Return (f r)
  fmap f (Effect m) =
    let monadFmap = fmap @m
    in Effect (monadFmap (fmap f) m)
  fmap f (Step next) =
    let functorFmap = fmap @f
    in Step (functorFmap (fmap f) next)

instance (Functor f, Monad m) => Monad (Stream f m) where
  (Return r) >>= k = k r
  (Effect m) >>= k = Effect (fmap (>>= k) m)
  (Step next) >>= k = Step (fmap (>>= k) next)

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  af <*> ax = af >>= (<$> ax)

empty :: Stream f m ()
empty = Return ()

effect :: Monad m => m r -> Stream f m r
effect eff = Effect $ Return <$> eff

yield :: a -> Stream (Of a) m ()
yield a = Step (a :> empty)

listToStream :: [e] -> Stream (Of e) m ()
listToStream [] = Return ()
listToStream (x : xs) = Step (x :> listToStream xs)

ssum :: (Num e, Monad m) => Stream (Of e) m r -> m (Of e r)
ssum (Return r) = pure (0 :> r)
ssum (Effect m) = m >>= ssum
ssum (Step (s :> strm)) = first (+s) <$> ssum strm

printStream :: (Show e, Show r) => Stream (Of e) IO r -> IO ()
printStream (Return r) = putStrLn $ "Result: " <> show r
printStream (Effect m) = do
  putStrLn "Run action: "
  strm <- m
  printStream strm
printStream (Step (s :> strm)) = do
  putStrLn $ "Intermediate element: " <> show s
  printStream strm

exampleStream :: Stream (Of Int) IO ()
exampleStream = do
  yield 1
  effect (putStrLn "yielding 1!")
  yield 2
  effect (putStrLn "yielding 2!")
  yield 3
  effect (putStrLn "yielding 3!")
  pure ()

maps :: (Functor f, Monad m) =>
        (forall x. f x -> g x) ->
        Stream f m r ->
        Stream g m r
maps converter (Return r) = Return r
maps converter (Effect m) =
  Effect (fmap (maps converter) m)
maps converter (Step strm) =
  Step (converter (fmap (maps converter) strm))

mapOf :: Monad m => (a -> b) ->
         Stream (Of a) m r ->
         Stream (Of b) m r
mapOf f = maps (first f)

zipsWith :: Monad m =>
            (forall x y p. (x -> y -> p) -> f x -> g y -> h p) ->
            Stream f m r ->
            Stream g m r ->
            Stream h m r
zipsWith op (Return r) _ = Return r
zipsWith op _ (Return r) = Return r
zipsWith op (Effect m) strm =
  Effect (fmap (flip (zipsWith op) strm) m)
zipsWith op strm (Effect m) =
  Effect (fmap (zipsWith op strm) m)
zipsWith op (Step fs) (Step gs) =
  Step (op (zipsWith op) fs gs)

           
