module Countdown where
import System.Random
import Data.List (delete, sortOn, transpose, intersperse, intercalate, findIndex)
import Data.List.Split
import Control.Monad.Trans.State
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

data Formula = Number Int
             | Plus Formula Formula 
             | Minus Formula Formula
             | Times Formula Formula
             | Divide Formula Formula
             deriving Show

lenghtFormula :: Formula -> Int
lenghtFormula (Number _)   = 0
lenghtFormula (Plus l r)   = 1 + lenghtFormula l + lenghtFormula r
lenghtFormula (Minus l r)  = 1 + lenghtFormula l + lenghtFormula r
lenghtFormula (Times l r)  = 1 + lenghtFormula l + lenghtFormula r
lenghtFormula (Divide l r) = 1 + lenghtFormula l + lenghtFormula r

-- | Possible numbers in the countdown problem ranging from 1 to 10 
-- and 4 high numbers
numbers :: [Formula]
numbers = fmap Number $ [5, 8, 4, 9, 50, 10]

-- | Evaluate the formula to the goal it want to reach
evaluate :: Int -> [Formula] -> [(Formula, Int)]
evaluate goal = fmap ev 
  where ev formula = let outcome = evaluateF formula 
                     in if outcome > goal then (formula, outcome - goal)
                        else (formula, goal - outcome)

-- | Evaluate each formula to its haskell operation counter part                     
evaluateF :: Formula -> Int 
evaluateF (Number x)     = x
evaluateF (Plus p1 p2)   = evaluateF p1 + evaluateF p2
evaluateF (Minus p1 p2)  = evaluateF p1 - evaluateF p2
evaluateF (Times p1 p2)  = evaluateF p1 * evaluateF p2
evaluateF (Divide p1 p2) = evaluateF p1 `quotZero` evaluateF p2

quotZero :: Int -> Int -> Int
quotZero _ 0 = 1000
quotZero n d = n `quot` d

randomNumber :: State StdGen Formula
randomNumber = fmap ((!!) numbers) (state (randomR (0, 5)))

randomOperation :: State StdGen (Formula -> Formula -> Formula)
randomOperation = fmap pickOperation (state $ randomR (0, 2))

pickOperation :: Int -> (Formula -> Formula -> Formula)
pickOperation n 
  | n == 0 = Plus
  | n == 1 = Minus
  | n == 2 = Times 


randomFormula :: Int -> State StdGen Formula
randomFormula depth = replicateM depth randomOperation >>= buildFormula

buildFormula :: [Formula -> Formula -> Formula] -> State StdGen Formula
buildFormula []             = randomNumber
buildFormula (operation:os) = do 
  let (l, r)   = splitHalf os 
  leftBranch  <- buildFormula l 
  rightBranch <- buildFormula r
  return $ operation leftBranch rightBranch

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

initialPopulation :: State StdGen [Formula]
initialPopulation = do 
  depths <- replicateM 20 (state $ randomR (1,10))
  traverse randomFormula depths

mutate :: [Formula] -> State StdGen [Formula]
mutate = traverse mutateFormula

mutateFormula :: Formula -> State StdGen Formula
mutateFormula formula = (state $ randomR (0, 1 :: Float)) >>= mutate formula
  where depth = lenghtFormula formula
        mutate f x
          | x > 0.05  = return formula
          | otherwise = (state $ randomR (1, depth)) >>= replaceSubformula f depth

replaceSubformula :: Formula -> Int -> Int -> State StdGen Formula          
replaceSubformula (Number x) _ _         = randomNumber
replaceSubformula formula depth n
  | n == 0     = randomFormula (depth - n)
  | otherwise  = replace formula 
                  (\l -> replaceSubformula l depth (n - 1)) 
                  (\r -> replaceSubformula r depth (n - 2))

replace 
  :: Formula  
  -> (Formula -> State StdGen Formula) 
  -> (Formula -> State StdGen Formula) 
  -> State StdGen Formula
replace (Plus l r) fl fr   = fl l >>= (\l' -> (\r' -> Plus l' r') <$> (fr r))
replace (Minus l r) fl fr  = fl l >>= (\l' -> (\r' -> Minus l' r') <$> (fr r))
replace (Times l r) fl fr  = fl l >>= (\l' -> (\r' -> Times l' r') <$> (fr r))
replace (Divide l r) fl fr = fl l >>= (\l' -> (\r' -> Divide l' r') <$> (fr r))

-- 
recombine :: Formula -> Int -> Formula -> Int -> (Formula, Formula)
recombine formula1 point1 formula2 point2 = (formula1, formula2)

split :: Int -> Formula -> (Formula, Maybe (Formula -> Formula))
split n (Number x) = (Number x, Nothing)
split n formula
  | n == 0    = (formula, Nothing)
  | otherwise = undefined 

selectParents :: [(Formula, Int)] -> State StdGen [(Formula, Formula)]
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

recombineParents :: [(Formula, Formula)] -> State StdGen [[Formula]]
recombineParents = traverse ((>>=) (state $ randomR (0, 100 :: Int)) . recombined)
  where recombined (f1, f2) n
          | n < 70    = return [f1, f2]
          | otherwise = do 
              p1 <- state $ randomR (0, lenghtFormula f1)
              p2 <- state $ randomR (0, lenghtFormula f2)
              let (f1', f2') = recombine f1 p1 f2 p2
              return [f1', f2']

tuple2 :: [a] -> (a, a)
tuple2 (h:hs:_) = (h, hs)
tuple2 _ = error "Can't create Tuple2 out of a list with < 2 elements"

selectNextGeneration :: [(Formula, Int)] -> [(Formula, Int)] -> [(Formula, Int)]
selectNextGeneration np op = take 100 $ sortOn snd $ np ++ op

evolution :: State StdGen (Formula, Int)
evolution = do
  population    <- initialPopulation
  let evaluated = evaluate 107 population
  evolved <- evolve evaluated
  return $ head evolved
    where evolve evaluated = do
            parents        <- selectParents evaluated
            recombined     <- recombineParents parents
            mutated        <- traverse mutate recombined
            let evaluated' = fmap (evaluate 107) mutated
                survivors  = selectNextGeneration (concat evaluated') (evaluated)
            if snd (head (survivors)) == 0 then
              return survivors
            else evolve survivors

main :: IO () 
main = do 
  stdGen <- getStdGen
  print (runState evolution stdGen)