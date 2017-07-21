{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Delay

#if defined(Variant_ts_7_lazyst) || defined(Variant_tl_7_lazyst)
import Control.Monad.ST.Lazy
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.STRef.Lazy
#endif

#if defined(Variant_ts_2_david_separate) || defined(Variant_ts_5_david2_separate) || defined(Variant_tl_2_david_separate) || defined(Variant_tl_5_david2_separate)
import qualified Separate
#endif

{-------------------------------------------------------------------------------
  The various variants
-------------------------------------------------------------------------------}

#if defined(Variant_ts_1_david) || defined(Variant_tl_1_david)
-- David's version
runDelayDavid :: Delay a -> (Progress, a)
runDelayDavid d = (p, r)
  where
    (p, r) = case d of
               Now a -> (Done, a)
               Later d' -> case runDelayDavid d' of (p', r') -> (NotYet p', r')
#endif

#if defined(Variant_ts_4_david2) || defined(Variant_tl_4_david2)
-- | Variation on David's version
runDelayDavid2 :: Delay a -> (Progress, a)
runDelayDavid2 d = (fst pr, snd pr)
  where
    pr = case d of
           Now a -> (Done, a)
           Later d' -> case runDelayDavid2 d' of (p, r) -> (NotYet p, r)
#endif

#if defined(Variant_ts_3_lazylet) || defined(Variant_tl_3_lazylet)
-- | Using lazy-let
runDelayLazyLet :: Delay a -> (Progress, a)
runDelayLazyLet (Now a)    = (Done, a)
runDelayLazyLet (Later d') = let ~(p', r') = runDelayLazyLet d' in (NotYet p', r')
#endif

#if defined(Variant_ts_6_explicit) || defined(Variant_tl_6_explicit)
-- explicitly written out lazy pattern match.
runDelayExplicit :: Delay a -> (Progress, a)
runDelayExplicit (Now a)    = (Done, a)
runDelayExplicit (Later d') = let p'r' = runDelayExplicit d'
                              in (NotYet (fst p'r'), snd p'r')
#endif

#if defined(Variant_ts_7_lazyst) || defined(Variant_tl_7_lazyst)
runDelayLazyST :: Delay a -> (Progress, a)
runDelayLazyST = \xs -> runST $ do
    r <- newSTRef xs
    x <- unsafeInterleaveST $ readSTRef r
    p <- next r
    return (p, runNow x)
  where
    next :: STRef s (Delay a) -> ST s Progress
    next r = do
      xs <- readSTRef r
      case xs of
        Now _   -> return Done
        Later d -> do writeSTRef r d
                      p' <- next r
                      return $ NotYet p'

    runNow :: Delay a -> a
    runNow (Now   a) = a
    runNow (Later d) = runNow d
#endif

{-------------------------------------------------------------------------------
  Main application

  We use CPP rather than something like

  > case someRuntimeFlag of
  >   ... -> callFirstVariant ..
  >   ... -> callOtherVariant ..

  to make absolutely sure ghc isn't floating something out and keeping it
  unnecessarily because it might somehow think that it might still be used.
-------------------------------------------------------------------------------}

-- | Return 0 but take n steps to do it
foo :: Int -> Delay Int
foo 0  = Now 0
foo !n = Later (foo (n - 1))

evalProgress :: Progress -> IO ()
evalProgress Done       = return ()
evalProgress (NotYet p) = evalProgress p

main :: IO ()
main = do
#if defined(Variant_ts_1_david)
    let !(p, finalResult) = runDelayDavid (foo 10000000)
#endif
#if defined(Variant_ts_2_david_separate)
    let !(p, finalResult) = Separate.runDelayDavid (foo 10000000)
#endif
#if defined(Variant_ts_3_lazylet)
    let !(p, finalResult) = runDelayLazyLet (foo 10000000)
#endif
#if defined(Variant_ts_4_david2)
    let !(p, finalResult) = runDelayDavid2 (foo 10000000)
#endif
#if defined(Variant_ts_5_david2_separate)
    let !(p, finalResult) = Separate.runDelayDavid2 (foo 10000000)
#endif
#if defined(Variant_ts_6_explicit)
    let !(p, finalResult) = runDelayExplicit (foo 10000000)
#endif
#if defined(Variant_ts_7_lazyst)
    let !(p, finalResult) = runDelayLazyST (foo 10000000)
#endif

#if defined(Variant_tl_1_david)
    let ~(p, finalResult) = runDelayDavid (foo 10000000)
#endif
#if defined(Variant_tl_2_david_separate)
    let ~(p, finalResult) = Separate.runDelayDavid (foo 10000000)
#endif
#if defined(Variant_tl_3_lazylet)
    let ~(p, finalResult) = runDelayLazyLet (foo 10000000)
#endif
#if defined(Variant_tl_4_david2)
    let ~(p, finalResult) = runDelayDavid2 (foo 10000000)
#endif
#if defined(Variant_tl_5_david2_separate)
    let ~(p, finalResult) = Separate.runDelayDavid2 (foo 10000000)
#endif
#if defined(Variant_tl_6_explicit)
    let ~(p, finalResult) = runDelayExplicit (foo 10000000)
#endif
#if defined(Variant_tl_7_lazyst)
    let ~(p, finalResult) = runDelayLazyST (foo 10000000)
#endif

    evalProgress p
    print finalResult
