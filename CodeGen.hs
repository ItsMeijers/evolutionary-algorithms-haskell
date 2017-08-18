module CodeGen where
  import System.Process (readProcessWithExitCode)
  import System.Exit
  import System.Random
  import Debug.Trace
  import BrainfuckInterpreter

  main :: IO ()
  main = do
    program <- initialise
    case checkSyntax $ parseSource (commands program) of
      Left _ -> main
      Right prg -> print prg


  type Programs = [Program]

  newtype Program = Program { commands :: String } deriving Show

  instance Evaluable Program where
    evaluate cs = do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "../brainfuck/bf" ["-e"] (commands cs)
      return $ if exitCode == ExitSuccess then
        scoreOutput "Hello World" stdout
      else 100


  scoreOutput :: String -> String -> Int
  scoreOutput [] []         = 0
  scoreOutput [] (_:xs)     = 92 + scoreOutput [] xs
  scoreOutput (_:xs) []     = 92 + scoreOutput [] xs
  scoreOutput (x:xs) (y:ys) = abs (fromEnum x - fromEnum y) + scoreOutput xs ys

  class Evaluable a where
    evaluate :: a -> IO Int

  getCommand :: Int -> Char
  getCommand 1 = '>'
  getCommand 2 = '<'
  getCommand 3 = '+'
  getCommand 4 = '-'
  getCommand 5 = '.'
  getCommand 6 = ','
  getCommand 7 = '['
  getCommand 8 = ']'

  -- initialise :: IO Programs
  initialise = do
     gen <- newStdGen
     return $ let (n, g) = randomR (1, 1000) gen
                  randomIndices = take n (randomRs (1,8) g)
              in Program $ fmap getCommand randomIndices

  selectParents :: Foldable f => f a -> IO (f a)
  selectParents = undefined

  recombine :: Foldable f => f a -> IO (f a)
  recombine = undefined

  mutate :: Foldable f => f a -> IO (f a)
  mutate = undefined

  evaluateCandidates :: Foldable f => Evaluable a => f a -> IO (f (a, Int))
  evaluateCandidates = undefined

  selectIndividuals :: Foldable f => f (a, Int) -> IO (f a)
  selectIndividuals = undefined

  evolve :: Foldable f => Evaluable a => f a -> IO (f a)
  evolve cs = do
    parents    <- selectParents cs
    recombined <- recombine parents
    mutated    <- mutate recombined
    evaluated  <- evaluateCandidates mutated
    selectIndividuals evaluated
