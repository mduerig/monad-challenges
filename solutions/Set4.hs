{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2 
  (Maybe(..)
  ,headMay
  ,tailMay
  ,lookupMay
  ,divMay
  ,maximumMay
  ,minimumMay)

--generalA ::   (a -> b) -> Gen a -> Gen b    
--transMaybe :: (a -> b) -> Maybe a -> Maybe b

--generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
--yLink ::    (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

--genTwo ::   Gen a -> (a -> Gen b) -> Gen b
--link ::   Maybe a -> (a -> Maybe b) -> Maybe b

-- genTwo ~= link, generalB ~= yLink

-- genTwo :: m a -> (a -> m b) -> m b
-- yLing :: (a -> b -> c) -> ma -> mb -> mc

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  mmap :: (a -> b) -> m a -> m b
  return :: a -> m a

instance Monad Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a
  mmap _ Nothing = Nothing
  mmap f (Just a) = Just (f a)
  return a = Just a
  
instance Monad [] where
  bind [] _ = []
  bind (a:as) f = f(a) ++ (bind as f)
  mmap _ [] = []
  mmap f (a:as) = f(a) : (mmap f as)
  return a = [a]

newtype Gen a = Gen (Seed -> (a, Seed))

instance Monad Gen where
  bind (Gen a) f = Gen (\s -> 
                      let (ra, t) = a s
                          (Gen b) = f ra
                      in  b t)
  mmap f (Gen a) = Gen (\s -> 
                      let (ra, t) = a s
                          b = f ra
                      in (b, t))
  return a = Gen (\s -> (a, s))

evalGen :: Gen a -> Seed -> a
evalGen (Gen ga) s = let (a, _) = ga s in a

-- repRandom
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = liftM2 (:) x (sequence xs)
--sequence (x:xs)  = x `bind` \x' -> 
--       (sequence xs) `bind` \xs' -> 
--                      return (x':xs')

-- generalB
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = return f `ap` ma `ap` mb
--liftM2 f ma mb = ma `bind` \a -> 
--                 mb `bind` \b -> 
--                     return (f a b)

-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

-- combine
join :: Monad m => m (m a) -> m a
join mma = mma `bind` id

-- combStep :: [a -> b] -> [a] -> [b]
ap :: Monad m => m (a -> b) -> m a -> m b
ap f ma = f `bind` \f' -> 
         ma `bind` \a -> 
           return (f' a)

--allCombs3
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = (return f) `ap` ma `ap` mb `ap` mc
--liftM3 f ma mb mc = (liftM2 f ma mb) `ap` mc
--liftM3 f ma mb mc = (liftM2 f ma mb) `bind` \f' -> 
--                                  mc `bind` \c ->
--                                    return (f' c)

-- Set 1 revisited

fiveRands :: [Integer]
fiveRands = evalGen gens seed 
  where gens = sequence $ take 5 $ repeat (Gen rand)
        seed = mkSeed 1

fiveRands2 :: [Integer]
fiveRands2 = let 
               gen = 
                 (Gen rand) `bind` \a ->
                 (Gen rand) `bind` \b ->
                 (Gen rand) `bind` \c ->
                 (Gen rand) `bind` \d ->
                 (Gen rand) `bind` \e ->
                 return [a, b, c, d, e]
               in 
                 evalGen gen $ mkSeed 1

randEven :: Gen Integer
randEven = (Gen rand) `bind` \a -> return (2 * a)

randOdd :: Gen Integer
randOdd = randEven `bind` \a -> return (a + 1)

randTen :: Gen Integer
randTen = (Gen rand) `bind` \a -> return (10 * a)

randLetter :: Gen Char
randLetter = (Gen rand) `bind` \a -> return (toLetter a)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair = liftM2 (,) 
--generalPair ga gb = ga `bind` \a ->
--                    gb `bind` \b -> return (a, b) 

randPair :: Gen (Char, Integer)
--randPair = randLetter `bind` \a ->
--           (Gen rand) `bind` \b -> return (a, b)
randPair = generalPair randLetter (Gen rand)


-- Set 2 revisited

queryGreek :: GreekData -> String -> Maybe Double
queryGreek values key = 
      let 
        xs = lookupMay key values
      in 
        xs `bind` \xs' ->
        tailMay xs' `bind` \txs ->
        maximumMay txs `bind` \maxTail ->
        headMay xs' `bind` \hxs ->
        divMay (fromIntegral maxTail) (fromIntegral hxs) `bind` \q ->
        return q
      
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries name1 name2 = 
  liftM2 (+) (lookupMay name1 salaries) (lookupMay name2 salaries)
--addSalaries salaries name1 name2 = 
--  lookupMay name1 salaries `bind` \s1 -> 
--  lookupMay name2 salaries `bind` \s2 ->
--    return (s1 + s2)
    
tailProd :: Num a => [a] -> Maybe a
tailProd xs = mmap product $ tailMay xs
--tailProd xs = tailMay xs `bind` \xs' -> 
--  return (product xs')

tailSum :: Num a => [a] -> Maybe a
tailSum xs = mmap sum $ tailMay xs

tailMax :: Ord a => Num a => [a] -> Maybe a
tailMax xs = tailMay xs `bind` maximumMay
-- tailMax xs = join $ mmap maximumMay $ tailMay xs

tailMin :: Ord a => Num a => [a] -> Maybe a
tailMin xs = tailMay xs `bind` minimumMay


-- Set 3 revisited

allPairs :: [a] -> [b] -> [(a,b)]
allPairs a b = a `bind` \a' -> 
               b `bind` \b' ->
               return (a',b')

data Card = Card Int String

instance Show Card where
  show (Card rank suit) = (show rank) ++ suit

allCards :: [Int] -> [String] -> [Card]
allCards rank suit = rank `bind` \r -> 
                     suit `bind` \s ->
                     return (Card r s)

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 a b = liftM2 (,) a b

allCards2 :: [Int] -> [String] -> [Card]
allCards2 rank suit = liftM2 Card rank suit

























