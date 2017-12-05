{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
  where 
    (a, s) = rand $ mkSeed 1
    (b, t) = rand $ s
    (c, u) = rand $ t
    (d, v) = rand $ u
    (e, w) = rand $ v

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g = \s ->
  let(r, t) = g s
  in(f r, t)

randLetter :: Gen Char
randLetter = generalA toLetter rand

randString3 :: String
randString3 = [a, b, c]
  where
    (a, s) = randLetter $ mkSeed 1
    (b, t) = randLetter $ s
    (c, u) = randLetter $ t    
  
randEven :: Gen Integer
randEven = generalA (*2) rand
  
randOdd :: Gen Integer
randOdd = generalA ((*2).(+1)) rand
  
randTen :: Gen Integer
randTen = generalA (*10) rand
  
randPair :: Gen (Char, Integer)
randPair = \s -> 
  let
    (c, t) = randLetter s
    (n, u) = rand t
  in ((c, n), u)
  
generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb = \s ->
  let
    (ra, t) = ga s
    (rb, u) = gb t
  in
    ((ra, rb), u)
    
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb = \s ->
  let
    (ra, t) = ga s
    (rb, u) = gb t
  in 
    (f ra rb, u)
  
generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
repRandom []        = \s -> ([], s)
repRandom (ga:gas)  = \s -> 
  let
    (ra, t) = ga s
    (ras, u) = repRandom gas t
  in
    (ra:ras, u)
    
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f = \s -> 
  let
    (ra, t) = ga s
  in
    f ra t

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = ga `genTwo` \a -> 
                    gb `genTwo` \b ->
                    mkGen (f a b)

mkGen :: a -> Gen a
mkGen a = \s -> (a, s)

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 []        = mkGen []
repRandom2 (ga:gas)  = genTwo ga (\a -> \t -> 
  let
    (ras, u) = repRandom2 gas t
  in 
    (a:ras, u))

repRandom3 :: [Gen a] -> Gen [a]
repRandom3 []        = mkGen []
repRandom3 (ga:gas)  = ga `genTwo` (\a -> 
         (repRandom3 gas) `genTwo` (\as -> 
         mkGen (a:as)))



























