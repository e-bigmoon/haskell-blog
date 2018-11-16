module Main (main) where

import           Gauge.Main
import           Gauge.Main.Options

import           Test.QuickCheck

import           SplitCC

main :: IO ()
main = do
  let conf = defaultConfig { displayMode = Condensed }
  sampleData1 <- generate $ vectorOf 10 charGen
  sampleData2 <- generate $ vectorOf 1000 charGen
  sampleData3 <- generate $ vectorOf 100000 charGen
  sampleData4 <- generate $ vectorOf 10000000 charGen

  defaultMainWith conf
    [ bgroup "splitCC" [ bench "10"       $ whnf splitCC sampleData1
                       , bench "1000"     $ whnf splitCC sampleData2
                       , bench "100000"   $ whnf splitCC sampleData3
                       , bench "10000000" $ whnf splitCC sampleData4
                       ]
    , bgroup "foldSplitCC" [ bench "10"       $ whnf foldSplitCC sampleData1
                           , bench "1000"     $ whnf foldSplitCC sampleData2
                           , bench "100000"   $ whnf foldSplitCC sampleData3
                           , bench "10000000" $ whnf foldSplitCC sampleData4
                           ]
    ]

charGen :: Gen Char
charGen = elements (['a'..'z']++['A'..'Z'])
