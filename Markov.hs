module Markov ( 
    Chain,
    buildChain,
    runChain,
    buildChainMultipleInput
    ) where

import Data.Map as Map hiding (foldr, map)
import Control.Monad.Random

type Chain a = Map a [a] 

buildChainMultipleInput :: (Ord a) => [[a]] -> Chain a
buildChainMultipleInput trainingData = foldr (Map.unionWith (++)) Map.empty $ map buildChain trainingData

buildChain :: (Ord a) => [a] -> Chain a
buildChain [a, b] = insert a [b] $ Map.singleton b []
buildChain (first:xs) = Map.insertWith (++) first [head xs] $ buildChain xs

runChain :: (RandomGen g, Ord a) => Chain a -> Rand g [a]
runChain chn = do
    firstWord <- choice (keys chn)
    runChain' chn firstWord

runChain' :: (RandomGen g, Ord a) => Chain a -> a -> Rand g [a]
runChain' chn word = do
    nextWord <- choice subsequentWords
    generatedChain <- runChain' chn nextWord
    return $ (word : generatedChain)
    where
        subsequentWords = if nextWords == [] 
            then keys chn 
            else nextWords
        nextWords = (Map.!) chn word

choice :: (RandomGen g) => [a] -> Rand g a
choice xs = do
    randomIndex <- getRandomR (0,(length xs)-1)
    return $ xs !! randomIndex
