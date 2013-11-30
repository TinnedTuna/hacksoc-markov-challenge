module Markov ( 
    Chain,
    buildChain,
    runChain,
    buildChainMultipleInput,
    buildChainPar
    ) where


import Control.Monad.Random
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import Data.Map as Map hiding (foldr, map)

type Chain a = Map a [a] 

buildChainMultipleInput :: (NFData a, Ord a) => [[a]] -> Chain a
buildChainMultipleInput trainingData = foldr (Map.unionWith (++)) Map.empty $ map buildChain trainingData

buildChainPar :: (NFData a, Ord a) => [[a]] -> Chain a
buildChainPar trainingData = force $ foldr (Map.unionWith (++)) Map.empty $ runEval $ do
    return $ parMap (rpar) (force . buildChain) trainingData

buildChain :: (NFData a, Ord a) => [a] -> Chain a
buildChain xs = force $ buildChain' xs Map.empty
    where 
    buildChain' :: (Ord a) => [a] -> Chain a -> Chain a
    buildChain' [a, b] c = insert a [b] $ c
    buildChain' (first:xs) c = buildChain' xs $ Map.insertWith (++) first [head xs] c

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
