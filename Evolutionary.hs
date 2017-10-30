module Evolutionary where
    import Data.List (sortOn)
    import System.Random
    import Control.Monad.Trans.State
    import Control.Monad (replicateM)

    type RandomS a = State StdGen a

    -- class Ord a => Evolutionary a where
    --     initialPopulation :: State StdGen [a]
    --     evaluate :: Num b => [a] -> [(a, b)]
    --     selectParents :: Num b => [(a, b)] -> State StdGen [(a, a)]
    --     recombineParents :: [(a, a)] -> State StdGen [[a]]
    --     mutate :: [a] -> State StdGen [a]
    --     selectNextGeneration :: Num b => [(a, b)] -> [(a, b)]
    --     terminationCondition :: Int -> (a, b) -> Bool

    -- evolve :: (Evolutionary a, Num b) => State StdGen (a, b)

    survivalSelection :: Ord b => Int -> [[(a, b)]] -> [(a, b)]
    survivalSelection n = concatMap (take n . sortOn snd)

    initialise :: Int -> State StdGen a -> State StdGen [a]
    initialise n state = replicateM n state

    evaluate :: Functor f => (a -> b) -> f a -> f (a, b)
    evaluate f = fmap (\a -> (a, f a))

    mutate :: (a -> State StdGen a) -> [a] -> State StdGen [a]
    mutate f = traverse f

    -- Int Seed point -> can be further randomized with mkStdGen if more random
        -- instances are needed
    -- Make f into ((Int, a, a) -> a) 
    recombine :: ((a, a) -> State StdGen a) -> [(a, a)] -> State StdGen [a]
    recombine f = traverse f 
    
    -- evolution :: Int -> RandomS a -> (a -> b) -> RandomS [a]
    -- evolution populationSize randomPopulation evaluationF = do 
    --     initialPopulation <- initialise populationSize randomPopulation
    --     let evaluated     =  evaluate evaluationF initialPopulation
    --     evolved           <- evolve evaluated
    --     return $ head evolved
    --         where evolve evaluated = do
    --             parents       <- selectParents evaluated
    --             recombined    <- recombineC parents
    --             mutated       <- traverse mutate recombined
    --             let evaluated' = fmap evaluate mutated
    --                 survivors  = sortOn snd (concat evaluated')
    --             if snd (head (survivors)) == 0 then
    --               return survivors
    --             else evolve (E.survivalSelection 2 evaluated')

    -- Generic operations on populations

    randomS :: Int -> Int -> State StdGen Int
    randomS x y = state $ randomR (x, y)

    tuple2 :: [a] -> (a, a)
    tuple2 (h:hs:_) = (h, hs)
    tuple2 _ = error "Can't create Tuple2 out of a list with < 2 elements"
  

    randomPairs :: [a] -> State StdGen (a, a)
    randomPairs xs = let n = length xs - 1 in do 
        r1 <- randomS 0 n
        r2 <- randomS 0 n
        return (xs !! r1, xs !! r2)

    flipPosition :: [a] -> State StdGen [a]
    flipPosition board = do
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


