import Data.Array.IO (newListArray, writeArray, readArray, IOUArray)
import Control.Monad (forM_, forM)
import Control.Applicative ((<$>))

import Data.Bits (setBit, (.&.), shiftR)
import qualified Data.List
import System.Environment (getArgs)

import System.Random

data Pair = P {pIndex :: !Int, pValue :: !Int}


type MyArray = IOUArray Int Int

-- сливает две упорядоченные последовательности в одну
merge [] ys = ys
merge xs [] = xs
merge xs'@(x:xs) ys'@(y:ys) = 
  if pValue x < pValue y then
    x : merge xs ys'
  else
    y : merge xs' ys

filterEvenI = go False
  where go True  (x:xs) = x : go False xs
        go False (_:xs) = go True xs
        go _ [] = []

-- сортировка элементов с индесами в массиве
sort :: MyArray -> [Pair] -> IO ()
sort arr indexes = do
  let sortedIndexes = merge (filterEvenI indexes) (filterEvenI $ undefined:indexes)
  sequence_ $ zipWith (\oldIP newIP -> writeArray arr (pIndex oldIP) (pValue newIP)) indexes sortedIndexes

-- минимальная число - степень двойки, большое или равное данному
ceilPowerOfTwo :: Int -> Int
ceilPowerOfTwo n = if n.&.(n-1) == 0 then n else roundN
  where roundN = setBit 0 (floorPower+1)
        floorPower = length $ takeWhile (>0) $ map (shiftR n) [1..]

pShellSort sourceList = do
  let n = length sourceList `div` 2
  let n' = ceilPowerOfTwo n
  
  arr <- newListArray (1, 2*n) sourceList
  let stepSeq = n' : map (`div` 2) stepSeq

  forM_ (takeWhile (>0) stepSeq) $ \step -> do
    let group i = [i, i+step..2*n]
    let groups = map group [1..step]
    pairGroups <- forM groups $ mapM (\i -> (P i) <$> (readArray arr i))
    forM_ pairGroups (sort arr)
  return arr


main = do
  count <- (read.head) <$> getArgs
  randomList <- (take count . randoms) <$> return (mkStdGen count) --return $ take count [100000,99999..]

  [_, method] <- getArgs
  case method of
    -- стандартный метод сортировки списков
    "standart" -> print $ Data.List.sort randomList 
    
    -- наша сортировка Шелла
    "custom" -> do
      arr <- pShellSort randomList
      mapM (readArray arr) [1..length randomList] >>= print
