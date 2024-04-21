{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl')
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified SD.Utility.MaxLengthSequence as MLSeq
import SD.Utility.MinMaxSequence (MMSeqSetting (..))
import qualified SD.Utility.MinMaxSequence as MMSeq
import qualified SD.Utility.Percentile as Perc
import System.IO (hFlush, stdout)
import System.Random

main :: IO ()
main = timePerc

-- | Checks how MinMaxSequence performs.
timeMinMaxSequence :: IO ()
timeMinMaxSequence = do
  putStrLn "Computing... " >> hFlush stdout
  let seqLen = 500
  let numRandom = 200000
  let result =
        foldl'
          ( \(sequ, sum') x ->
              let sequ' = MMSeq.append x sequ
                  max' = fromJust $ MMSeq.lookup sequ'
               in (sequ', sum' + max')
          )
          (fromJust $ MMSeq.empty seqLen MMSeqMax, 0 :: Int)
          (take numRandom $ randoms (mkStdGen 0) :: [Int])
  putStrLn $ "done; sum of max values: " ++ show (snd result)

-- | Checks how Percentile performs.
timePerc :: IO ()
timePerc = do
  putStr "Computing... " >> hFlush stdout
  let percLen = 250
      numRandom = 1000000
      result =
        foldl'
          ( \(!accPerc, !sum') x ->
              let accPerc' = Perc.append x accPerc
                  sum'' = sum' + fromMaybe 0 (Perc.lookup accPerc')
               in (accPerc', sum'')
          )
          (fromJust $ Perc.empty percLen, 0 :: Double)
          -- Random prices between 20 and 100.
          (take numRandom $ randomRs (20, 100) (mkStdGen 0) :: [Double])
  putStrLn $ "done; sum: " ++ show (snd result)

-- | Checks how the naive alternative to Percentile performs.
timePercNaive :: IO ()
timePercNaive = do
  putStr "Computing... " >> hFlush stdout
  let percLen = 250
      numRandom = 1000000
      result =
        foldl'
          ( \(!accMlSequ, !sum') x ->
              let accMlSequ' = MLSeq.append x accMlSequ
                  tailPerc =
                    if MLSeq.full accMlSequ'
                      then
                        ( fromIntegral
                            . Seq.length
                            . Seq.filter (< (fromJust $ MLSeq.tail accMlSequ'))
                            $ MLSeq.sequ accMlSequ'
                        )
                          / fromIntegral (MLSeq.length accMlSequ')
                      else 0
                  sum'' = sum' + tailPerc
               in (accMlSequ', sum'')
          )
          (fromJust $ MLSeq.empty percLen, 0 :: Double)
          -- Random prices between 20 and 100.
          (take numRandom $ randomRs (20, 100) (mkStdGen 0) :: [Double])
  putStrLn $ "done; sum: " ++ show (snd result)
