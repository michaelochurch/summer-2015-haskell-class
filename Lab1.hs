module Lab1 where

import Data.Char
import System.Random


-- TODO: fix this to be something used in the 1-player game (4 lives). 
-- Declaring a constant, `nLives`.

nLives :: Int
nLives = 4


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

rollResult :: [Int] -> RollResult
rollResult dice =
  case dice of
   [die1, die2] -> if die1 == 1 || die2 == 1
                   then if die1 == die2 then SnakeEyes else OneRolled
                   else if die1 == die2 then Doubles die1 else Normal die1 die2
   _            -> error "rollResult expects two dice"

-- Exercise #1: Read up on `case/of` and pattern matching. Rewrite this function
-- using a `case/of` block and no `if` statements. 


-- Defining an *action*. "IO" means that the action is capable of stateful
-- effects. An `IO a` is an action that returns a value of type `a`. 

rollD6 :: IO Int
rollD6 = randomRIO (1, 6)


-- Exercise #2: why is rolling a die stateful? Why does it have the
-- type signature `IO Int`?

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

-- If you're not familiar with `Maybe`, it's a *parameterized* union type.
--     `data Maybe a = Nothing | Just a`
-- Typically Nothing represents null, an  "n/a" value, or failure. Unlike Java's
-- null, however, it's type-safe. 

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
      putStrLn "Bad input! I only understand Y/N."
      humanYN prompt

humanNoChoice :: String -> IO Bool
humanNoChoice prompt = do
  putStr prompt
  input <- getLine
  case readYN input of
   Just False -> do
     putStrLn "Well you're doing it anyway."  -- "But thou must!"
     return True
   _ -> return True

-- `blankLine` is an action that prints an empty line to the console. It does IO
-- but has nothing to return, so it returns `()`, pronounced "unit". 
blankLine :: IO ()
blankLine = putStrLn ""


-- A simple product type with record syntax. 
data RoundResult = RoundResult {pointsScored :: Int,
                                livesLost    :: Int} deriving Show

-- This is the workhorse method. It plays a round of Dodgy Dice.
-- TODO: explain the ($) operator. 
playARound :: IO RoundResult
playARound = loop 0
  where loop score = do
          cont <- if score /= 0
                  then humanYN "Roll again? "
                  else humanNoChoice "Forced roll: "
          if cont
            then do
              dice <- roll2d6
              putStrLn $ "You rolled: " ++ (show dice)
              case rollResult dice of
               SnakeEyes -> return $ RoundResult {pointsScored = 0, livesLost = 2}
               OneRolled -> return $ RoundResult {pointsScored = 0, livesLost = 1}
               Normal d1 d2 -> loop (score + d1 + d2)
               Doubles d    -> loop (score + 4 * d)
            else return (RoundResult {pointsScored = score,
                                      livesLost    = 0})


playAGame :: Int -> IO ()
playAGame nLives = do
  putStrLn "Welcome to Dodgy Dice!"
  loop nLives 0
    where loop lives score =
            if lives <= 0
            then putStrLn ("GAME OVER: Your score is " ++ (show score))
            else do
              blankLine >> putStrLn "New Round!" >> blankLine
              printStatus lives score
              RoundResult ptsScored lvsLost <- playARound
              if lvsLost > 0
                then putStrLn ("You lost " ++ (show lvsLost) ++ " lives.")
                     >> loop (lives - lvsLost) score
                else putStrLn ("You scored " ++ (show ptsScored) ++ " points.")
                     >> loop lives (score + ptsScored)
          printStatus lives score =
            putStrLn $ "Lives left: " ++ (show lives) ++ " Score: " ++ (show score)

-- Exercise #6: Currently, when you lose a life on rolling a 1, you see this at
-- the console:
          
--    "You lost 1 lives."

-- How would you change the code so that it says "You lost 1 life."?


-- A Haskell executable, when run, executes an action called `main` with type
-- `IO ()`.

main :: IO ()
main = playAGame nLives
