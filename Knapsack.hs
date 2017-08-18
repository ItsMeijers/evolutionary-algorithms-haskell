module Knapsack where
  import Data.Foldable (maximumBy)
  import Data.Ord (comparing)
  import System.Random

  main :: IO ()
  main = undefined

  data Item = Item { cost :: Float, value :: Float, included :: Bool }

  type Population = [[Item]]
  type Parents    = [([Item], [Item])]
  type Evaluated  = [([Item], Float)]

  evaluate :: Population -> Evaluated
  evaluate = fmap evaluateIndividual
    where
      evaluateIndividual individual =
        let (newCs, score, _) = foldr addScores ([], 0.0, 0.0) individual
        in (newCs, score)
      addScores item (xs, tScore, tCost) =
        if included item && tCost + cost item <= 100 then
          (item : xs, tScore + value item * cost item, tCost + cost item)
        else
          (item : xs, tScore, tCost) -- TODO set included item false

  selectParents :: Evaluated -> Parents
  selectParents = undefined

  recombine :: Parents -> Population
  recombine = undefined

  mutate :: Population -> StdGen -> Population
  mutate pop gen = fmap mutateL pop

  selectIndividuals :: Evaluated -> Evaluated
  selectIndividuals = id

  evolve :: Evaluated -> Float -> Int -> Evaluated
  evolve evaluatedCandidates bestSolution noImprovement =
    let nec = selectIndividuals . evaluate . mutate . recombine . selectParents
        nec' = nec evaluatedCandidates
        best = snd $ maximumBy (comparing snd) nec'
    in if best > bestSolution then evolve nec' best 0
       else if noImprovement == 30 then nec'
       else evolve nec' bestSolution (noImprovement + 1)
