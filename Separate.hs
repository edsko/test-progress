{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE CPP #-}

module Separate (runDelay) where

import Delay

#if defined(Alg_david_separate)
-- David's original version, now in a separate module
runDelay :: Delay a -> (Progress, a)
runDelay d = (p, r)
  where
    (p, r) = case d of
               Now a -> (Done, a)
               Later d' -> case runDelay d' of (p', r') -> (NotYet p', r')
#endif

#if defined(Alg_david2_separate)
-- | David's variation, now in a separate module
runDelay :: Delay a -> (Progress, a)
runDelay d = (fst pr, snd pr)
  where
    pr = case d of
           Now a -> (Done, a)
           Later d' -> case runDelay d' of (p, r) -> (NotYet p, r)
#endif
