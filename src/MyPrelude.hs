-- This file is part of JEST.
-- 
-- JEST is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
-- 
-- JEST is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the LICENSE for more details.
-- 
-- A copy of the GNU General Public License should have been included
-- along with JEST in a file named LICENSE. If not, see
-- <http://www.gnu.org/licenses/>.


-- | some random utility functions
module MyPrelude where

import Control.Monad.Except
import Control.Monad (when, liftM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (map)
import qualified Data.Traversable as Traversable (sequence)
import Control.Arrow
import Data.List
import Data.Monoid
import Data.Int

joinMaybeMonoid :: Monoid a => Maybe a -> a
joinMaybeMonoid Nothing  = mempty
joinMaybeMonoid (Just a) = a

inflist x = x:inflist x

swap (a,b) = (b,a)

ifM :: Monad m => Bool -> a -> a -> m a
ifM g t f = if g then return t else return f

throwIf :: (MonadError e m) => Bool -> e -> m ()
throwIf g e = when g $ throwError e

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = 
  do r <- mapM f xs 
     return $ concat r
     
concatM :: Monad m => m [[a]] -> m [a]
concatM = liftM concat
     
intMapM :: (Monad m) => (a -> m b) -> IntMap a -> m (IntMap b)
intMapM f m = Traversable.sequence $ IntMap.map f m
  
zipM :: (Monad m) => m [a] -> m [b] -> m [(a,b)]
zipM = liftM2 zip
                  
foldMM :: (Monad m) => (a -> b -> m a) -> m a -> m [b] -> m a
foldMM f ma mbs = do
  a <- ma
  bs <- mbs
  foldM f a bs

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (_:xs) = maybeLast xs

safeTail :: [a] -> [a]
safeTail as = case as of
               [] -> []
               (_:as') -> as'

promoteMaybePair :: (Maybe a, Maybe b) -> Maybe (a, b)
promoteMaybePair (ma, mb) = do a <- ma
                               b <- mb
                               return (a, b)
                               
satisfyMaybe :: (a -> Bool) -> a -> Maybe a
satisfyMaybe p a = if p a then Just a else Nothing
                                  
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- | Disjoint union on lists. If xs =/= ys, xs `dunion` ys === Just $ xs ++
-- ys; Nothing otherwise
dunion :: Eq a => [a] -> [a] -> Maybe [a]
dunion = dunionBy (==)

-- | Disjoint union on lists with an explicit equality predicate. If
-- xs =/= ys, xs `dunion` ys === Just $ xs ++ ys; Nothing otherwise
dunionBy :: (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
dunionBy eq xs ys = let res = xs ++ ys
                    in if length (nubBy eq res) == length res then Just res
                       else Nothing

singleton :: a -> [a]
singleton a = [a]

int2int32 :: Int -> Int32
int2int32 = fromInteger . toInteger
