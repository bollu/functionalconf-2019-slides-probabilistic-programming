\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[english]{babel}
\usepackage{minted}
\usepackage{comment}
%https://tex.stackexchange.com/questions/365292/how-to-use-non-ascii-chars/365303#365303
\usepackage{pmboxdraw}

\newenvironment{code}{\VerbatimEnvironment \begin{minted}{haskell}}{\end{minted}}
\newenvironment{ascii}{\VerbatimEnvironment \begin{minted}{text}}{\end{minted}}
\newcommand{\hsmint}[1]{\mintinline{haskell}{#1}}

\begin{document}


\begin{comment}
\begin{code}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import System.Random
import System.Environment (getArgs)
import Debug.Trace
import Control.Applicative
import Data.List(sort, nub)
import Data.Proxy
import Control.Monad (replicateM)
import Data.Monoid hiding(Ap)
import Control.Monad
import Data.Bits
import GHC.Exts
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Data.Map as M
\end{code}
\end{comment}


\title{My Presentation}
\author{Siddharth Bhat}
\institute{IIIT Hyderabad}
\date{\today}



\begin{frame}[fragile]{Motivation}

\begin{code}
-- | fair dice
dice :: Rand Int
dice = do
  u <- uniform01
  floor (7*u)


tossDice :: Rand Int
tossDice = do
    d1 <- dice
    d2 <- dice
    return $ d1 + d2
\end{code}

\input{"| cabal v2-exec slides -- tossDice"}

If the dice roll is prime, 

\begin{code}
tossDicePrime :: Rand Int
tossDicePrime = do
    d <- tossDice
    score $ if prime d then 1 else 0
    return $ d
\end{code}

\input{"| cabal v2-exec slides -- tossDicePrime"}
  
\end{frame}

\begin{frame}[fragile]
\begin{code}
data Rand x where
    -- | Lift a pure value
    Ret :: x -> Rand x
    -- | pick an integer uniformly in the range [lo, hi]
    Choose :: (Int, Int) -> (Int -> Rand x) -> Rand x
    -- | Change the probability of a given value
    Score :: Double -> Rand x -> Rand x

choose :: (Int, Int) -> Rand Int
choose (lo, hi) = Choose (lo, hi) Ret

score :: Double -> Rand ()
score s = Score s (Ret ())

-- | Uniform distributinon on (0, 1)
uniform01 :: Rand Double
uniform01 = do
    x <- choose (0, 65535)
    return $ x / 65535.0
\end{code}
\end{frame}

\begin{comment}
\begin{code}
instance Functor Rand where
  fmap f (Ret x) = Ret (f x)
  fmap f (Choose lohi next) = Choose lo hi (\r -> fmap f (next r))
  fmap f (Score s mx) = Score s (fmap f mx)
instance Applicative Rand where
  pure = Ret
  (<*>) = ap 
instance Monad Rand where
  return = Ret
  (Ret x) >>= x2my = x2my x
  (Choose lohi r2mx) >>= x2my = Choose lohi (\r -> r2mx r >>= x2my)
  (Score s mx) >>= x2my = Score s (mx >>= x2my)
\end{code}
\end{comment}

\begin{frame}[fragile]
\begin{code}
-- | Run the computation _unweighted_.
-- | Ignores scores.
sample :: RandomGen g => g -> Rand a -> (a, g)
sample g (Ret a) = (a, g)
sample g (Choose lohi next) =
  let (r, g') = randomR lohi g in sample g'(next r)
sample g (Score f mx) = sample g mx -- Ignore score
\end{code}
\end{frame}

\begin{frame}[fragile]
MCMC methods
\end{frame}

\begin{frame}[fragile]
\begin{code}
-- | Trace all random choices made when generating this value
data Trace a =
  Trace { tval :: a, -- ^ The value itself
          tscore :: Double, -- ^ The total score
          trs :: [Int] -- ^ The ranom numbers used
        }
-- | Lift a pure value into a Trace value
mkTrace :: a -> Trace a
mkTrace a = Trace a 1.0 []
-- | multiply a score to a trace
scoreTrace :: Double -> Trace a -> Trace a
scoreTrace f Trace{..} = Trace{tscore = tscore * f, ..}
-- | Prepend randomness
recordRandomness :: Int -> Trace a -> Trace a
recordRandomness r Trace{..} = Trace { trs = r:trs, ..}

-- | Trace a random computation.
-- We know what randomness is used
traceR :: Rand x -> Rand (Trace x)
traceR (Ret x) = Ret (mkTrace x)
traceR (Choose lohi next) = do
  r <- choose lohi
  trx <- traceR $ mx r
  return $ recordRandomness r $ trx
traceR (Score s mx) = do
  trx <- traceR $ mx
  return $ scoreTrace s $ trx
\end{code}
\end{frame}


-- | Return a trace-adjusted MH computation
\begin{frame}[fragile]
\begin{code}

  
mhStep :: Rand (Trace x) -- ^ proposal
         -> Trace x -- ^ current position
         -> Rand (Trace x)
mhStep r trace = do
  -- | Return the original randomness, perturbed
  rands' <- perturbRandomness (trs trace)
  -- | Run the original computation with the perturbation
  trace' <- feedRandomness rands' r
  let ratio = traceAcceptance trace' / traceAcceptance trace
  -- | sample a random float from (0, 1)
  r <- (choose (0, 100)) / 100
  return $ if r < ratio then trace' else trace
  
traceAcceptance :: Trace x -> Double
traceAcceptance tx =
  tscore tx * fromIntegral (length (trs tx))
  
perturbRandomness :: [((Int, Int), Int)] -> Rand [Int]
perturbRandomness rands = do
  ix <- choose (0, (length rands-1)) -- ^ Random index
  let ((lo, hi), _) = rands !! ix
  r <- choose (lo, hi)
  -- | Replace random index w/ random val.
  return $ replaceListAt ix ((lo, hi), r) rands 

-- | run the Rand with the randomness provided, and then
-- return the rest of the proabilistic computation
feedRandomness :: [Double] -> Rand a -> Rand a
feedRandomness (r:rs) (Choose lohi next) = feedRandomness rs (next r)
feedRandomness rs (Score s mx) = Score s $ feedRandomness rs mx
feedRandomness _ r = r
\end{code}
\end{frame}

\begin{frame}[fragile]
\begin{code}
-- | Find a starting position that does not have probability 0
findNonZeroTrace :: Rand (Trace x) -> Rand (Trace x)
findNonZeroTrace tracedR = do
  trace <- tracedR
  if tscore trace /= 0
  then return $ trace
  else findNonZeroTrace tracedR

-- | run the computatation after taking weights into account
weighted ::  Int -> Rand x -> Rand [x]
weighted 0 _ = return []
weighted n r =
  let tracedR = traceR r
      -- go :: Int -> Rand (Trace x) -> Rand (Trace [x])
      go 0 _ = return []
      go n tx = do
        tx' <- repeatM 10 (mhStep tracedR) $ tx
        txs <- go (n-1) tx'
        return (tx:txs)
  in do
      seed <- findNonZeroTrace $ tracedR
      tracedRs <- go n seed 
      return $ map tval tracedRs
\end{code}
\end{frame}

\begin{frame}[fragile]{Payoff!}
\begin{code}
predictCoinBias :: [Int] -> Rand Double
predictCoinBias flips = do
  -- | Start with uniform prior
  b <- uniform01

  forM_ flips $ \f -> do
    -- | Maximum a posterior
    score $ if f == 1 then b else (1 - b)
  return $ b
\end{code}

\begin{code}
predictCoinBiasNoData :: Rand Double
predictCoinBiasNoData = predictCoinBias []
\end{code}

\input{"| cabal v2-exec slides -- predictCoinBiasNoData"}


\begin{code}
predictCoinBias0 :: Rand Double
predictCoinBias0 = predictCoinBias [0]
\end{code}

\input{"| cabal v2-exec slides -- predictCoinBias0"}

\begin{code}
predictCoinBias01 :: Rand Double
predictCoinBias01 = predictCoinBias [0, 1]
\end{code}

\input{"| cabal v2-exec slides -- predictCoinBias01"}

\end{frame}




\begin{comment}



\begin{code}

compose :: Int -> (a -> a) -> (a -> a)
compose 0 f = id
compose n f = f . compose (n - 1) f

-- | Utility library for drawing sparklines

-- | List of characters that represent sparklines
sparkchars :: String
sparkchars = "▁▂▃▄▅▆▇█"

-- Convert an int to a sparkline character
num2spark :: Show a => RealFrac a => a -- ^ Max value
  -> a -- ^ Current value
  -> Char
num2spark maxv curv =
  if curv > maxv
  then error  $ "curv" <> (show curv) <> ">" <> "maxv" <> (show maxv)
  else sparkchars !! (floor $ (curv / maxv) * (fromIntegral (length sparkchars - 1)))

series2spark :: Show a => RealFrac a => a -> [a] -> String
series2spark maxv vs = map (num2spark maxv) vs

-- Probabilites
-- | A way to choose uniformly. Maybe slightly biased due to an off-by-one ;)

-- | Replace the element of a list at a given index
replaceListAt :: Int -> a -> [a] -> [a]
replaceListAt ix a as = let (l, r) = (take (ix - 1) as, drop ix as)
                         in l ++ [a] ++ r


-- | Repeat monadic computation N times
repeatM :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatM 0 f x = return x
repeatM n f x = f x >>= repeatM (n - 1) f



-- | Create a histogram from values.
histogram :: Int -- ^ number of buckets
          -> Double
          -> Double
          -> [Double] -- values
          -> [Int]
histogram nbuckets minv maxv as =
    let
        perbucket = (maxv - minv) / (fromIntegral nbuckets)
        bucket v = floor $ (v - minv) / perbucket
        startBuckets :: M.Map Int Int
        startBuckets = M.fromList $ [(i, 0) | i <- [bucket minv..bucket maxv]]
        bucketed :: M.Map Int Int
        bucketed = foldl (\m v -> M.insertWith (+) (bucket v) 1 m) startBuckets as
     in map snd . M.toList $ bucketed


printHistogram :: Int -> Double -> Double -> [Double] -> IO ()
printHistogram nbuckets minv maxv samples =
  let histValues = (map fromIntegral . histogram nbuckets minv maxv $  samples) :: [Double]
  in putStrLn $ series2spark (maximum histValues) histValues


diagramR :: RandomGen g => g -> Rand a -> D.Diagram D.B
diagramR g a = D.circle 1


latexWrapper :: IO () -> IO ()
latexWrapper printer = do
    putStrLn "\\begin{ascii}"
    printer
    putStrLn ""
    putStrLn "\\end{ascii}"

divisors :: Int -> [Int]
divisors n = [i | i <- [1..n-1], n `mod` i == 0]

prime :: Int -> Bool
prime 1 = False
prime p = length (divisors p) == 1

main :: IO ()
main = do
    let g = mkStdGen 1
    args <- getArgs
    case args !! 0  of
        "tossDice" -> do
          let (samples, _) = sample g (replicateM  2000 tossDice)
          latexWrapper $ print $ take 10 samples
          latexWrapper $ printHistogram 10 2 12 $ (map fromIntegral samples)
          
        "tossDicePrime" -> do
          let (samples, _) = sample g (weighted 2000 tossDicePrime)
          latexWrapper $ print $ take 10 $ samples
          latexWrapper $ printHistogram 10 2 12 $ (map fromIntegral samples)
          
        "predictCoinBiasNoData" -> do 
            let (mcmcsamples, _) = sample g (weighted 2000 predictCoinBiasNoData)
            latexWrapper $ printHistogram 10 0 0.9 $ take 2000 $ mcmcsamples
            
        "predictCoinBias0" -> do 
            let (mcmcsamples, _) = sample g (weighted 2000 predictCoinBias0)
            latexWrapper $ printHistogram 10 0 0.9 $ take 2000 $ mcmcsamples
            
        "predictCoinBias01" -> do 
            let (mcmcsamples, _) = sample g (weighted 2000 predictCoinBias01)
            latexWrapper $ printHistogram 10 0 0.9 $ take 2000 $ mcmcsamples

        "bar" -> putStrLn $ "bar"
        _ -> putStrLn $ "unknown"


\end{code}

\end{comment}

\end{document}
