{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"  
  show (Just a) = "Just " ++ show a

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:_) = Just a

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:as) = Just as

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a0 ((a, b):xs) 
          | a0 == a = Just b
          | otherwise = lookupMay a0 xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:y:xs) = maximumMay ((max x y):xs)
           
minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:y:xs) = minimumMay ((min x y):xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek values key =
  let xs      = lookupMay key values
      xsTail  = case xs of
        Just xs -> tailMay xs
        Nothing -> Nothing
      maxTail = case xsTail of
        Just xs -> maximumMay xs
        Nothing -> Nothing
      xsHead = case xs of
        Just xs -> headMay xs
        Nothing -> Nothing
  in case (maxTail, xsHead) of
    (Just t, Just h) -> divMay (fromIntegral t) (fromIntegral h)
    otherwise -> Nothing

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 values key = 
  let xs      = lookupMay key values
      xsTail  = xs `link` tailMay
      maxTail = xsTail `link` maximumMay
      xsHead  = xs `link` headMay
  in
    maxTail `link` \t -> 
    xsHead `link` \h -> 
      divMay (fromIntegral t) (fromIntegral h)
  

queryGreek3 :: GreekData -> String -> Maybe Double
queryGreek3 values key = 
  let xs      = lookupMay key values
      xsTail  = chain tailMay xs
      maxTail = chain maximumMay xsTail
      xsHead  = chain headMay xs
  in
    chain (\t -> 
      chain (\h -> 
        divMay (fromIntegral t) (fromIntegral h)) xsHead) maxTail
  
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries values key1 key2 = 
  let s1 = lookupMay key1 values
      s2 = lookupMay key2 values
  in case (s1, s2) of
    (Just s1, Just s2) -> Just (s1 + s2)
    otherwise -> Nothing
    
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = ma `link` \a -> 
                mb `link` \b -> 
                  Just (f a b)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 values key1 key2 = 
  yLink (+) (lookupMay key1 values) (lookupMay key2 values)

tailProd :: Num a => [a] -> Maybe a
tailProd xs = (tailMay xs) `link` \x -> Just $ product x

tailSum :: Num a => [a] -> Maybe a
tailSum xs = (tailMay xs) `link` \x -> Just $ sum x

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma = ma `link` \a -> Just $ f a

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 xs =  transMaybe product (tailMay xs)

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 xs =  transMaybe sum (tailMay xs)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = transMaybe maximumMay (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine mma = mma `link` id

tailMax2 :: Ord a => [a] -> Maybe a
tailMax2 = combine . tailMax
 
tailMax3 :: Ord a => [a] -> Maybe a
tailMax3 xs = (tailMay xs) `link` maximumMay















