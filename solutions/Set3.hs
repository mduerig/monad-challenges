{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (a:as) (b:bs) = 
  (a,b) :
  (allPairs [a] bs) ++ 
  (allPairs as [b]) ++ 
  (allPairs as bs)
  
data Card = Card Int String 

instance Show Card where
  show (Card ranks suit) = (show ranks) ++ suit

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (a:as) (b:bs) = 
  (Card a b) :
  (allCards [a] bs) ++ 
  (allCards as [b]) ++
  (allCards as bs)
  
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f [] _ = []
allCombs f _ [] = []
allCombs f (a:as) (b:bs) =
  (f a b) :
  (allCombs f [a] bs) ++ 
  (allCombs f as [b]) ++
  (allCombs f as bs)

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs' Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
--allCombs3 f as bs = allCombs ($) (allCombs f as bs)
--allCombs3 f as bs = allCombs' ($) (allCombs' f as bs)
--allCombs3 f a b = combStep (combStep (combStep [f] a) b)
allCombs3 f a b c = [f] `combStep` a `combStep` b `combStep` c

combStep :: [a -> b] -> [a] -> [b]
combStep _ [] = []
combStep [] _ = []
combStep (f:fs) (a:as) = 
  (f a) :
  (combStep [f] as) ++
  (combStep fs [a]) ++
  (combStep fs as)
  
allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
--allCombs' f a = combStep (combStep [f] a)
allCombs' f a b = [f] `combStep` a `combStep` b



















  







