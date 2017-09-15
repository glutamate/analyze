module Main where

import Control.Monad
import Data.Function ((&))
import System.Random

import Criterion.Main

-- import Analyze
import Analyze.Frame as Frame
-- import Analyze.Series as Series

n, k :: Int
n = 1000 -- 2e9
k = 100

benchmark1 :: Frame Int String -> IO ()
benchmark1 _ = undefined


groupingBenchmarks :: Frame Int String -> [Benchmark]
groupingBenchmarks df =
    [ bench "1e7 elements" (nfIO $ benchmark1 df)
    ]


sample :: [a] -> Int -> IO [a]
sample coll size = forM [1 .. size] $ \_ -> do
        n' <- randomRIO (0, length coll - 1) :: IO Int
        return (coll !! n')


makeBenchmarkFrame :: IO (Frame Int String)
makeBenchmarkFrame = do
    putStrLn "Generating data frame for the benchmark..."
    id1 <- sample ( mappend "id" . show <$> [1..k] ) n
    id2 <- sample ( mappend "id" . show <$> [1..k] ) n
    id3 <- sample ( mappend "id" . show <$> [1..n `div` k] ) n
    id4 <- fmap show <$> sample [1..k] n
    id5 <- fmap show <$> sample [1..k] n
    id6 <- fmap show <$> sample [1..n `div` k] n
    v1  <- fmap show <$> sample [1..5 :: Int] n
    v2  <- fmap show <$> sample [1..5 :: Int] n
    v3  <- [1..n]
           & fmap (const $ show <$> (randomRIO (0, 100) :: IO Int))
           & sequence
    Frame.ofColumns
        [ ("id1", id1)
        , ("id2", id2)
        , ("id3", id3)
        , ("id4", id4)
        , ("id5", id5)
        , ("id6", id6)
        , ("v1", v1)
        , ("v2", v2)
        , ("v3", v3)
        ]


main :: IO ()
main = do
  df <- makeBenchmarkFrame
  defaultMain
    [ bgroup "Grouping" (groupingBenchmarks df)
    ]
