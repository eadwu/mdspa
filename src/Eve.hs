module Eve
( module Prelude
, (|>), (<|), (>>>), (<<<)
, first, second
, mapLeft, foldMapM )
where
  -- Basically just copied from https://cronokirby.com/posts/2020/11/haskell-in-haskell-1/
  -- Renamed from Ourlude to Eve since Eve sounds better

  import Prelude
  import Data.Bifunctor ( first, second )

  -- What I mean by `forward`, is that if we take an expression like f (g (x)),
  -- i.e. f $ g $ x, or f <| g <| x, we have x first `entering` g, and then
  -- `entering` f.
  infixr 0 <| -- Since it's an alias for ($), just use its fixity
  (<|) :: (a -> b) -> a -> b
  (<|) = ($)
  {-# INLINE (<|) #-}

  -- By contrast x |> g |> f has the data moving forwards, or left to right.
  infixl 1 |>
  (|>) :: a -> (a -> b) -> b
  x |> f = f x
  {-# INLINE (|>) #-}

  -- Similarly \x -> f (g x) becomes f <<< g, or g >>> f, and the same remarks apply.
  infixr 9 <<< -- Since it's an alias for (.), just use its fixity
  (<<<) :: (b -> c) -> (a -> b) -> (a -> c)
  g <<< f = g . f
  {-# INLINE (<<<) #-}

  infixl 9 >>>
  (>>>) :: (a -> b) -> (b -> c) -> (a -> c)
  f >>> g = g . f
  {-# INLINE (>>>) #-}

  -- first :: (a -> c) -> (a, b) -> (c, b)
  -- second :: (b -> c) -> (a, c) -> (a, c)

  -- Transform an either by mapping on its left side
  mapLeft :: (e -> e') -> Either e a -> Either e' a
  mapLeft f = either (f >>> Left) Right

  -- Map over a list monadically, then squash the results monoidally
  foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
  foldMapM f = mapM f >>> fmap mconcat
