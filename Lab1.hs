module Lab1 where

import Data.Char
import System.Random


-- TODO: fix this to be something used in the 1-player game (4 lives). 
-- Declaring a constant, `nRounds`.

nRounds :: Int
nRounds = 10


-- Defining a datatype, `RollResult`.

data RollResult = SnakeEyes | OneRolled | Doubles Int | Normal Int Int
                deriving (Eq, Show)

-- This is an *algebraic data type*. Don't worry about the terminology just
-- yet. It's a tagged union that might be:
--   * the constant, `SnakeEyes`
--   * the constant, `OneRolled`
--   * a `Doubles` with an associated `Int`
--   * a `Normal` with two associated `Int`s (like an ordered pair).


-- Defining a function.

rollResult :: (Int, Int) -> RollResult
rollResult (die1, die2) =
  if die1 == 1
  then if die2 == 1 then SnakeEyes else OneRolled
  else if die1 == die2 then Doubles die1 else Normal die1 die2

-- Exercise 1: Read up on `case/of` and pattern matching. Rewrite this function
-- using a `case/of` block and no `if` statements. 


-- Defining an *action*. "IO" means that the action is capable of stateful
-- effects. An `IO a` is an action that returns a value of type `a`. 

rollD6 :: IO Int
rollD6 = randomRIO (1, 6)


-- Exercise #2: why is rolling a die stateful?

-- Exercise #3: what other conceivable type signatures could "roll a 6-sided
-- die" have?



-- Combining actions with `do` notation.

-- In English, "perform `rollD6` and call its return value `x1`, then perform
-- `rollD6` (again) and call its return value `x2`, then return `[x1, x2]`".

-- This will be covered more formally in Lecture #3. 

roll2d6 :: IO [Int]
roll2d6 = do
  x1 <- rollD6
  x2 <- rollD6
  return [x1, x2]


-- Another function. A return of `Nothing` represents failure while a return of
-- `Just b` means a successful parse of `Bool` `b`. 

readYN :: String -> Maybe Bool
readYN str =
  case head (map toUpper str) of
    'Y' -> Just True
    'N' -> Just False
    _   -> Nothing

-- Exercise #4: What do `head` and `map` do?
-- Hint #1: Hoogle is your friend.
-- Hint #2: in Haskell, `String` is a synonym for `[Char]`.


-- A simple action to get human yes/no input.
-- Note the recursion on user error. 
humanYN :: String -> IO Bool
humanYN prompt = do
  putStr prompt
  input <- getLine
  case readYN input of
    Just b -> return b
    Nothing -> do
      print "Bad input! I only understand Y/N."
      humanYN prompt


-- A simple product type with record syntax. 
data RoundResult = RoundResult {pointsScored :: Int,
                                livesLost    :: Int}

playARound :: IO RoundResult
playARound = loop 0
  where loop score = do
          continue <- if score /= 0
                      then humanYN "Roll again? "
                      else return True
          if continue
          then do

          else return (RoundResult {pointsScored = score,
                                    livesLost    = 0})

-- playARound :: IO 
-- playARound ai = loop 0
--   where loop score = undefined



  --                    do
   
  --         if score /= 0
  --         then humanYN "Roll again? "
  --         else return True



  -- where loop hScore cScore hPlaying cPlaying = do
  --         hContinue <- if hPlaying
  --                      then if hScore /= 0
  --                           then humanYN "Roll again? "
  --                           else return True
  --                      else return False
  --         cContinue <- if cPlaying
  --                      then if cScore /= 0
  --                           then runAI ai cScore
  --                           else return True
  --                      else return False
  --         if hContinue || cContinue
  --           then do
  --             dice <- roll2d6
  --             putStrLn ("Rolled " ++ (show dice))
  --             case (rollResult dice) of
  --               SnakeEyes    ->
  --                 return (RoundResult (if hContinue then (-2 * hScore) else hScore)
  --                                     (if cContinue then (-2 * cScore) else cScore))
  --               RolledOne    ->
  --                 return (RoundResult (if hContinue then 0 else hScore)
  --                                     (if cContinue then 0 else cScore))
  --               Normal d1 d2 ->
                  
  --           else return (RoundResult hScore cScore)
          


-- playARound uses `where` to define a subfunction. 

-- playARound :: IO RoundResult
-- playARound = loop 0 0
--   where
    

-- This is an action that 


main :: IO ()
main = do
  print $ "Welcome to Dodgy Dice!"
  x1 <- rollD6
  x2 <- rollD6
  print $ "You rolled a: " ++ show (x1 + x2)
