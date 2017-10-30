module Knapsack where
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import System.Random
import Data.List (delete, sortOn, transpose, intersperse, intercalate)
import Data.List.Split
import Control.Monad.Trans.State
import Control.Monad (replicateM)

data Item     = Item { cost :: Float, value :: Float, included :: Bool } 
type Knapsack = [Item]

main :: IO ()
main = runEvolution <$> getStdGen >>= printKnapsack

printKnapsack :: (Knapsack, Float) -> IO ()
printKnapsack (ks, score) = putStrLn ("Cost: "   ++ show (sum (fmap cost ks)) 
                                    ++ " Value: " ++ show (sum (fmap value ks)) 
                                    ++ " Score: " ++ show score)

runEvolution :: StdGen -> (Knapsack, Float)
runEvolution = evalState evolution

evolution :: State StdGen (Knapsack, Float)
evolution = do 
  population <- initialPopulation  
  let evaluated = evaluate population
  evolved <- evolve (evaluated, 0, 0)
  return $ head evolved
    where evolve (evaluated, noImprovement, score) = do
            parents    <- selectParents evaluated
            recombined <- recombineParents parents
            mutated    <- traverse mutate recombined
            let evaluated' = fmap evaluate mutated
                survivors  = reverse $ sortOn snd (concat evaluated')
                newScore   = snd (head survivors)
            if score >= newScore && noImprovement == 25 then
              return survivors
            else 
              if score >= newScore 
                then evolve (selectNextGeneration (concat evaluated') evaluated, noImprovement + 1, score)
              else evolve (selectNextGeneration (concat evaluated') evaluated, 0, score)

initialPopulation :: State StdGen [Knapsack]
initialPopulation = replicateM 500 $ replicateM 100 randomItem
  where randomItem = do 
          cost     <- state $ randomR (0, 1) 
          value    <- state $ randomR (0, 1) 
          included <- state $ random 
          return $ Item cost value included

evaluate :: [Knapsack] -> [(Knapsack, Float)]
evaluate = fmap (\k -> (k, knapsackScore k)) . correctKnapsacks
    where correctKnapsacks = 
            fmap (fst . foldl (\(ks, tCost) i -> 
                    if cost i + tCost < 100 
                      then (ks ++ [i], cost i + tCost) 
                    else (ks ++ [Item (cost i) (value i) False], 100)) ([], 0))

knapsackScore :: Knapsack -> Float
knapsackScore = foldr itemScore 0
  where itemScore item score = if included item 
                                  then score + value item * cost item 
                               else score

-- | Returns a list of 2 parents that are selected by tournament selection in Random pairs
selectParents :: [(Knapsack, Float)] -> State StdGen [(Knapsack, Knapsack)]
selectParents = fmap (paired . fmap select) . replicateM 500 . randomPairs
    where paired = fmap tuple2 . chunksOf 2
          select ((c1, s1), (c2, s2))
            | s1 > s2   = c1
            | otherwise = c2

randomPairs :: [a] -> State StdGen (a, a)
randomPairs xs = let n = length xs - 1 in do 
    r1 <- state $ randomR (0, n)
    r2 <- state $ randomR (0, n)
    return (xs !! r1, xs !! r2)
                  
recombineParents :: [(Knapsack, Knapsack)] -> State StdGen [[Knapsack]]
recombineParents = traverse ((>>=) (state $ randomR (0, 100 :: Int)) . recombined)
  where recombined (p1, p2) n
          | n > 70   = return [p1, p2]
          | otherwise = crossover p1 p2

crossover :: Knapsack -> Knapsack -> State StdGen [Knapsack]        
crossover p1 p2 = fmap (split p1 p2) (state $ randomR (1, length p1 - 1))
  where split p1 p2 n   = let (xs, xss) = splitAt n p1 
                              (ys, yss) = splitAt n p2
                          in [xs ++ yss, ys ++ xss]

mutate :: [Knapsack] -> State StdGen [Knapsack]
mutate = traverse (traverse flipIncluded)
    where flipIncluded item = flip item <$> (state $ randomR (0, 10 :: Int))
          flip item n
            | n < 1     = Item (cost item) (value item) (not $ included item)
            | otherwise = item

selectNextGeneration :: [(Knapsack, Float)] -> [(Knapsack, Float)] -> [(Knapsack, Float)]
selectNextGeneration newPopulation _ = newPopulation

tuple2 :: [a] -> (a, a)
tuple2 (h:hs:_) = (h, hs)
tuple2 _ = error "Can't create Tuple2 out of a list with < 2 elements"

