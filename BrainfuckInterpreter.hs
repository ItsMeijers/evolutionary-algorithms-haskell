module BrainfuckInterpreter where
  import Data.Maybe (mapMaybe)
  import qualified Data.List.NonEmpty as N

  data Command = GoRight
               | GoLeft
               | Increment
               | Decrement
               | Print
               | Read
               | LoopL
               | LoopR

  newtype BfProgram = BfProgram { statements :: [Command] }

  instance Show BfProgram where
    show = fmap showCommand . statements
      where showCommand GoRight   = '>'
            showCommand GoLeft    = '<'
            showCommand Increment = '+'
            showCommand Decrement = '-'
            showCommand Print     = '.'
            showCommand Read      = ','
            showCommand LoopL     = '['
            showCommand LoopR     = ']'

  parseSource :: String -> BfProgram
  parseSource xs = BfProgram $ mapMaybe readCommand xs
    where readCommand '>' = Just GoRight
          readCommand '<' = Just GoLeft
          readCommand '+' = Just Increment
          readCommand '-' = Just Decrement
          readCommand '.' = Just Print
          readCommand ',' = Just Read
          readCommand '[' = Just LoopL
          readCommand ']' = Just LoopR
          readCommand _   = Nothing

  data SyntaxError = UnclosedParentheses Int
                   | UnopenedParentheses Int
                   deriving Show

  -- | Program, Opening Positions, Closing Positions,
  -- opening parentheses, closing parentheses, current position
  type PC = ([Command], [Int], [Int], Int, Int, Int)

  emptyPC :: PC
  emptyPC = ([], [], [], 0, 0, 0)

  -- TODO Fix error message not right index
  checkSyntax :: BfProgram -> Either SyntaxError BfProgram
  checkSyntax = construct . foldr checkParens emptyPC . statements
    where checkParens command (pr, op, cp, o, c, cr) = case command of
              LoopL -> (LoopL:pr, cr:op, cp, o + 1, c, cr + 1)
              LoopR -> (LoopR:pr, op, cr:cp, o, c + 1, cr + 1)
              x     -> (x:pr, op, cp, o, c, cr + 1)

  -- TODO Fix error message not right index
  construct :: PC -> Either SyntaxError BfProgram
  construct (pr, op, cp, o, c, _)
    | o == c = if all (uncurry (>)) (zip op cp) then Right (BfProgram pr) else Left $ UnclosedParentheses 1
    | o > c  = Left $ UnclosedParentheses (last op)
    | o < c  = Left $ UnopenedParentheses (head $ drop o cp)

  data Tape a = Tape (N.NonEmpty a) a (N.NonEmpty a)

  emptyTape :: Tape Int
  emptyTape = Tape zeros 0 zeros
    where zeros = N.repeat 0

  moveRight :: Tape a -> Tape a
  moveRight (Tape (l N.:| ls) p (r N.:| (rs:rss))) =
    Tape (p N.:| l:ls) r (rs N.:| rss)

  moveLeft :: Tape a -> Tape a
  moveLeft (Tape (l N.:| (ls:lss)) p (r N.:| rs)) =
    Tape (ls N.:| lss) l (p N.:| (r:rs))
