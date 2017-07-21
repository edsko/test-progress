{-# LANGUAGE CPP #-}

module Main (main) where

import Delay

#if defined(Alg_lazyst)
import Control.Monad.ST.Lazy
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.STRef.Lazy
#endif

#if defined(Alg_david_separate) || defined(Alg_david2_separate)
import Separate
#endif

{-------------------------------------------------------------------------------
  The various variants
-------------------------------------------------------------------------------}

#if defined(Alg_david)
-- David's version
runDelay :: Delay a -> (Progress, a)
runDelay d = (p, r)
  where
    (p, r) = case d of
               Now a -> (Done, a)
               Later d' -> case runDelay d' of (p', r') -> (NotYet p', r')
#endif

#if defined(Alg_david2)
-- | Variation on David's version
runDelay :: Delay a -> (Progress, a)
runDelay d = (fst pr, snd pr)
  where
    pr = case d of
           Now a -> (Done, a)
           Later d' -> case runDelay d' of (p, r) -> (NotYet p, r)
#endif

#if defined(Alg_lazylet)
-- | Using lazy-let
runDelay :: Delay a -> (Progress, a)
runDelay (Now a)    = (Done, a)
runDelay (Later d') = let ~(p', r') = runDelay d' in (NotYet p', r')
#endif

#if defined(Alg_explicit)
-- explicitly written out lazy pattern match.
runDelay :: Delay a -> (Progress, a)
runDelay (Now a)    = (Done, a)
runDelay (Later d') = let p'r' = runDelay d'
                              in (NotYet (fst p'r'), snd p'r')
#endif

#if defined(Alg_lazyst)
runDelay :: Delay a -> (Progress, a)
runDelay = \xs -> runST $ do
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
foo 0 = Now 0
foo n = Later (foo (n - 1))

evalProgress :: Progress -> IO ()
evalProgress Done       = return ()
evalProgress (NotYet p) = evalProgress p

main :: IO ()
main = do
#if defined(Top_ts)
{-
  NOTE: This is NOT the same as

  > let !(p, finalResult) = runDelay (foo 10000000)

  (at least, not with -O0)
-}
    case runDelay (foo 10000000) of
      (p, finalResult) -> do
        evalProgress p
        print finalResult
#endif
#if defined(Top_tl)
    let ~(p, finalResult) = runDelay (foo 10000000)
    evalProgress p
    print finalResult
#endif
