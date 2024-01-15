module Ourlude (
    module Prelude, 
    (|>), 
    (<|),
    (>>>),
    (<<<),
    foldMapM,
    mapLeft,
    first,
    second
) where

import Prelude
import Data.Bifunctor (first, second)

-- forward composition operator
infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

-- backward composition operator
infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
g <<< f = g . f
{-# INLINE (<<<) #-}

infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}

-- map over a list monadically, then squash results monodically
foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat

-- transform an either by mapping on its left side
mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either ( f >>> Left) Right

