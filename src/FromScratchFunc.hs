module FromScratchFunc where

import Data.Bifunctor
import Data.Functor.Compose

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

-- λ> ssum $ each [1..3 :: Int]

printStream :: (Show e, Show r) => Stream (Of e) IO r -> IO ()
printStream (Return r) = putStrLn $ "Result: " <> show r
printStream (Effect m) = do
  putStrLn "Run action: "
  strm <- m
  printStream strm
printStream (Step (s :> strm)) = do
  putStrLn $ "Intermediate element: " <> show s
  printStream strm

-- λ> printStream stream1

showStream :: (Monad m, Show a, Show r) => Stream (Of a) m r -> m String
showStream strm =
  let showStream' (Return r) = pure ""
      showStream' (Effect m) = m >>= showStream'
      showStream' (Step (s :> esses)) =
        fmap ((show s <> ",") <>) (showStream' esses)
  in do
    s <- showStream' strm
    pure (take (length s - 1) s)

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

zipPair :: Monad m =>
           Stream (Of a) m r ->
           Stream (Of b) m r ->
           Stream (Of (a,b)) m r
zipPair = let pairUp p (e1 :> x1) (e2 :> x2) =
                (e1, e2) :> p x1 x2
          in zipsWith pairUp

zips :: (Monad m, Functor f, Functor g) =>
        Stream f m r ->
        Stream g m r ->
        Stream (Compose f g) m r
zips = let combiner p fx gy =
             Compose (fmap (\x -> fmap (p x) gy) fx)
       in zipsWith combiner

decompose :: (Monad m, Functor f) =>
             Stream (Compose m f) m r ->
             Stream f m r
decompose (Return r) = Return r
decompose (Effect m) = Effect (decompose <$> m)
decompose (Step (Compose mstrm)) = Effect $ do
  strm <- mstrm
  pure (Step (decompose <$> strm))

mapsM :: (Monad m, Functor f, Functor g) =>
         (forall x. f x -> m (g x)) ->
         Stream f m r ->
         Stream g m r
mapsM fun = decompose . maps (Compose . fun)

withEffect :: Monad m =>
              (e -> m ()) ->
              Stream (Of e) m r ->
              Stream (Of e) m r
withEffect eff = let go p@(e :> _) = eff e >> pure p
                 in mapsM go

-- λ> ssum $ withEffect print $ each [1..3 :: Int]
-- λ> ssum $ withEffect print $ withEffect (\_ -> putStr "Element: ") $ each [1..3 :: Int]

splitsAt :: (Monad m, Functor f) =>
            Int ->
            Stream f m r ->
            Stream f m (Stream f m r)
splitsAt n strm 
  | n > 0 = case strm of
              Return r -> Return (Return r)
              Effect m -> Effect (fmap (splitsAt n) m)
              Step f -> Step (fmap (splitsAt (n - 1)) f)
  | otherwise = Return strm

chunksOf :: forall f m r. (Monad m, Functor f) =>
            Int ->
            Stream f m r ->
            Stream (Stream f m) m r
chunksOf n strm =
  let cutChunk strm = fmap (chunksOf n) (splitsAt (n - 1) strm)
  in case strm of
    Return r -> Return r
    Effect m -> Effect (fmap (chunksOf n) m)
    Step fs -> Step (Step (fmap cutChunk fs))

-- λ> ssum $ withEffect print $ mapsM ssum $ chunksOf 2 $ each [1,1,1,1,1 :: Int]
