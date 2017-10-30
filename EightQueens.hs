module EightQueens where
import System.Random
import Data.List (delete, sortOn, transpose, intersperse, intercalate)
import Data.List.Split
import Control.Monad.Trans.State
import Control.Monad (replicateM)
import qualified Evolutionary as E

type Board = [Int]

main :: IO ()
main = runEvolution <$> getStdGen >>= printBoard

runEvolution :: StdGen -> Board
runEvolution = fst . evalState evolution

printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . intercalate "|") . transpose . fmap createColumn
  where createColumn = intersperse "---" . take 8 . flip fmap [1..] . createQueen
        createQueen pos i
          | i == pos  = " Q "
          | otherwise = "   "

evolution :: State StdGen (Board, Int)
evolution = do
  population    <- initialPopulation
  let evaluated = evaluate population
  evolved <- evolve evaluated
  return $ head evolved
    where evolve evaluated = do
            parents        <- selectParents evaluated
            recombined     <- recombineParents parents
            mutated        <- traverse mutate recombined
            let evaluated' = fmap evaluate mutated
                survivors  = selectNextGeneration evaluated'
            if snd (head (survivors)) == 0 then
              return survivors
            else evolve survivors

-- | Creates a State instance for generating a random chess board based on max
initialPopulation :: State StdGen [Board]
initialPopulation = replicateM 100 $ replicateM 8 (state $ randomR (1, 8))

-- | Evaluates a population of chessboards, adding the score to the chessboard
evaluate :: [Board] -> [(Board, Int)]
evaluate = fmap (\a -> (a, checkingQueens a))

checkingQueens :: Board -> Int
checkingQueens = sum . countCheck . zip [1..]
  where countCheck xs          = fmap (\x -> checked x (delete x xs)) xs
        checked xy             = length . filter (aligns xy)
        aligns (x, y) (x', y') = abs (x - x') == abs (y - y') || y == y'

-- | Selects the parents based on best 2 out of 5        
selectParents :: [(Board, Int)] -> State StdGen [(Board, Board)]
selectParents bs = select bs <$> replicateM 500 (state $ randomR (0, nParents))
  where 
    nParents  = length bs - 1
    select bs = fmap (tuple2 . fmap fst . sortOn snd . fmap (bs !!)) . chunksOf 5

tuple2 :: [a] -> (a, a)
tuple2 (h:hs:_) = (h, hs)
tuple2 _ = error "Can't create Tuple2 out of a list with < 2 elements"

-- | Combines two pair of parents to two children
recombineParents :: [(Board, Board)] -> State StdGen [[Board]]
recombineParents = traverse recombine
  where recombine (xs, ys) = crossover xs ys <$> state (randomR (1, length xs))
        crossover xs ys p  = [vs, ws, xs, ys]
          where (_, vs, ws) = foldr (buildOffsprings p) (0, [], []) (zip xs ys)

buildOffsprings :: Int -> (a, a) -> (Int, [a], [a]) -> (Int, [a], [a])
buildOffsprings p (x, y) (n, xs', ys')
      | p < n     = (n + 1, x : xs', y : ys')
      | otherwise = (n + 1, y : xs', x : ys')

-- | Mutates each chess board by a probability of 90%      
mutate :: [Board] -> State StdGen [Board]
mutate = traverse flipPosition
  where flipPosition board = do
          change <- state $ randomR (1, 10)
          if change < (9 :: Int) then do
            position    <- state $ randomR (0, 7)
            positionTwo <- state $ randomR (0, 7)
            return (swap position positionTwo board)
          else return board

swap :: Int -> Int -> [a] -> [a]
swap f s xs = zipWith (\x y ->
  if x == f then xs !! s
  else if x == s then xs !! f
  else y) [0..] xs

-- | Selects the next generation based on best out 2
selectNextGeneration :: [[(Board, Int)]] -> [(Board, Int)]
selectNextGeneration = sortOn snd . concatMap (take 2 . sortOn snd)
