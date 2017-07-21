{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE CPP #-}

module Separate where

import Delay

#if defined(Variant_ts_2_david_separate) || defined(Variant_tl_2_david_separate)
-- David's original version, now in a separate module
runDelayDavid :: Delay a -> (Progress, a)
runDelayDavid d = (p, r)
  where
    (p, r) = case d of
               Now a -> (Done, a)
               Later d' -> case runDelayDavid d' of (p', r') -> (NotYet p', r')
#endif

#if defined(Variant_ts_5_david2_separate) || defined(Variant_tl_5_david2_separate)
-- | David's variation, now in a separate module
runDelayDavid2 :: Delay a -> (Progress, a)
runDelayDavid2 d = (fst pr, snd pr)
  where
    pr = case d of
           Now a -> (Done, a)
           Later d' -> case runDelayDavid2 d' of (p, r) -> (NotYet p, r)
#endif
