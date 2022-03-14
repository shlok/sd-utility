{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified SD.Utility.MinMaxSequence as MMSeq
import System.IO (hFlush, stdout)
import System.Random (mkStdGen, randoms)

main :: IO ()
main = timeMinMaxSequence

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
                  max' = fromJust $ MMSeq.lookupMax sequ'
               in (sequ', sum' + max')
          )
          (fromJust $ MMSeq.empty seqLen, 0 :: Int)
          (take numRandom $ randoms (mkStdGen 0) :: [Int])
  putStrLn $ "done; sum of max values: " ++ show (snd result)
