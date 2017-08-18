module EightQueens where
  import System.Random
  import Data.List (delete, sortOn, transpose, intersperse, intercalate)
  import Data.List.Split
  import Control.Monad.Trans.State
  import Control.Monad (replicateM)

  type Board = [Int]

  printBoard :: Board -> IO ()
  printBoard = mapM_ (putStrLn . intercalate "|") . transpose . fmap createColumn
    where createColumn = intersperse "---" . take 8 . flip fmap [1..] . createQueen
          createQueen pos i
            | i == pos  = " Q "
            | otherwise = "   "

  main :: IO ()
  main = runEvolution <$> getStdGen >>= printBoard

  runEvolution :: StdGen -> Board
  runEvolution = fst . evalState evolution

  evolution :: State StdGen (Board, Int)
  evolution = do
    population    <- initialise 100 8
    let evaluated = evaluate population
    evolved <- evolve evaluated
    return $ head evolved
      where evolve evaluated = do
              parents       <- selectParents evaluated
              recombined    <- recombineC parents
              mutated       <- traverse mutate recombined
              let evaluated' = fmap evaluate mutated
              if snd (head (sortOn snd (concat evaluated'))) == 0 then
                return (sortOn snd (concat evaluated'))
              else evolve (survivalSelection evaluated')

  -- | Creates a State instance for generating a random chess board based on max
  initialise :: Int -> Int -> State StdGen [Board]
  initialise n max = replicateM n $ replicateM max (state $ randomR (1, max))

  -- initialise :: Monad f , Foldable f, RandomGen r => PopulationSize (Int) -> (StdGen -> a) -> State r (f a)

  evaluate :: [Board] -> [(Board, Int)]
  evaluate = fmap (\b -> (b, checkingQueens b))

  -- evaluate :: Foldable f , Num b => (a -> b) -> f a -> f (a, b)

  checkingQueens :: Board -> Int
  checkingQueens = sum . countCheck . zip [1..]
   where countCheck xs = fmap (\x -> checked x (delete x xs)) xs
         checked xy = length . filter (aligns xy)
         aligns (x, y) (x', y') = abs (x - x') == abs (y - y') || y == y'

  mutate :: [Board] -> State StdGen [Board]
  mutate = traverse flipPosition
    where flipPosition board = do
            change <- state $ randomR (1, 10)
            if change < (9 :: Int) then do
              position <- state $ randomR (0, 7)
              positionTwo <- state $ randomR (0, 7)
              return (swap position positionTwo board)
            else return board

  -- mutate :: Foldable f, RandomGen r -> (a -> a) -> f a -> State r (f a)

  swap :: Int -> Int -> [a] -> [a]
  swap f s xs = zipWith (\x y ->
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs


  -- recombine :: Foldable f, RandomGen r -> (a -> a) -> f (a, a) -> State r (f (f a))
  recombineC :: [(Board, Board)] -> State StdGen [[Board]]
  recombineC = traverse recombine

  recombine :: (Board, Board) -> State StdGen [Board]
  recombine ([], []) = return []
  recombine (xs, ys) = crossover <$> state (randomR (1, length xs))
    where crossover p = [vs, ws, xs, ys]
            where (_, vs, ws) = foldr (buildOffsprings p) (0, [], []) (zip xs ys)

  buildOffsprings :: Int -> (a, a) -> (Int, [a], [a]) -> (Int, [a], [a])
  buildOffsprings p (x, y) (n, xs', ys')
        | p < n     = (n + 1, x : xs', y : ys')
        | otherwise = (n + 1, y : xs', x : ys')

  -- selectParents :: Foldable f, Num b, RandomGen r => (f (a, b) -> f (a, a)) -> f (a, b) -> State (r b) (f (a, a))
  selectParents :: [(Board, Int)] -> State StdGen [(Board, Board)]
  selectParents bs = select <$> replicateM 500 (state $ randomR (0, length bs - 1))
    where select = map (tuple2 . map fst . sortOn snd . fmap (bs !!)) . chunksOf 5

  tuple2 :: [a] -> (a, a)
  tuple2 (h:hs:_) = (h, hs)
  tuple2 _ = error "Can't create Tuple2 out of a list with < 2 elements"

  -- survivalSelection :: Foldable f, Num b => (f a -> f a) -> (f (f (a, b))) -> f (a, b)
  survivalSelection :: [[(a, Int)]] -> [(a, Int)]
  survivalSelection = concatMap (take 2 . sortOn snd)
