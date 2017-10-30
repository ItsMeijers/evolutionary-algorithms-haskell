module GenericEightqueens where
    import qualified Evolutionary as E
    import System.Random 
    import Control.Monad.Trans.State
    import Control.Monad (replicateM)

    type Board = [Int]

    initialise :: Int -> State StdGen [Board]
    initialise n = E.initialise n $ replicateM 8 (state $ randomR (1, 8))

    mutate :: [Board] -> State StdGen [Board]
    mutate = E.mutate E.flipPosition

    