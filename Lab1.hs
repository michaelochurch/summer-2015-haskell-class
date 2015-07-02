module Main where

-- Import statements: necessary to use functions from modules like Data.Char
-- (for toUpper) and System.Random (for randomRIO).

import Data.Char
import System.IO
import System.Random


-- Declaring a constant, `nLives`.

defaultNLives :: Int
defaultNLives = 4


-- Defining a function, `oneThirdRoundingUp`, that will be used later on.

oneThirdRoundingUp :: Int -> Int
oneThirdRoundingUp n = (n + 2) `quot` 3


-- Defining an *action*. "IO" means that the action is capable of stateful
-- effects. An `IO a` is an action that returns a value of type `a`.

rollD6 :: IO Int
rollD6 = randomRIO (1, 6)


-- Exercise #1: why is rolling a die stateful? Why does it have the
-- type signature `IO Int`?

-- Exercise #2: what other conceivable type signatures could "roll a 6-sided
-- die" have?


-- Defining a datatype, `RollResult`.

data RollResult = SnakeEyes | OneRolled | Doubles Int | Normal Int Int
                deriving (Eq, Show)

-- This is an *algebraic data type*. It's a tagged union that might be:
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


scoreResult :: RollResult -> Int
scoreResult rr = case rr of
  SnakeEyes    -> 0
  OneRolled    -> 0
  Normal d1 d2 -> d1 + d2
  Doubles d    -> 4 * d

lifeLoss :: RollResult -> Int
lifeLoss rr = case rr of
  SnakeEyes    -> 2
  OneRolled    -> 1
  Normal _ _   -> 0
  Doubles _    -> 0

endsRound :: RollResult -> Bool
endsRound rr = (lifeLoss rr) /= 0


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
-- Typically `Nothing` represents null, an  "n/a" value, or failure. Unlike Java's
-- null, however, it's type-safe.

readYN :: String -> Maybe Bool
readYN "" = Nothing
readYN str =
  case head (map toUpper str) of
    'Y' -> Just True
    'N' -> Just False
    _   -> Nothing

-- Exercise #3: What do `head` and `map` do?
-- Hint #1: Hoogle is your friend.
-- Hint #2: in Haskell, `String` is a synonym for `[Char]`.


flush :: IO ()
flush = hFlush stdout

-- A simple action to get human yes/no input.
-- Note the recursion on user error.
humanYN :: String -> IO Bool
humanYN prompt = do
  putStr prompt
  flush
  input <- getLine
  case readYN input of
    Just b -> return b
    Nothing -> do
      putStrLn "Bad input! I only understand Y/N."
      humanYN prompt

-- This asks the user for input but ignores it.
humanNoChoice :: String -> IO Bool
humanNoChoice prompt = do
  putStr prompt
  flush
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


-- `playARound` is the workhorse method. It plays a round of Dodgy Dice.

-- Aside #1: You'll often see the infix operator `$`. This is the function
-- application operator, with the following type signature:

--   `($) :: (a -> b) -> a -> b`

-- Its purpose is explained in the Course Slides (Lab #1).

playARound :: Int -> IO RoundResult
playARound roundNum = loop 0 1
  where loop score rollNum = do
          cont <- if rollNum > oneThirdRoundingUp roundNum
                  then humanYN "Roll again (y/n)? "
                  else humanNoChoice "Forced roll: "
          if cont
            then do
              dice <- roll2d6
              let result = rollResult dice
              putStrLn $ "You rolled: " ++ show dice ++ " (" ++ show (scoreResult result) ++ " pts.)"
              if endsRound result
                then return $ RoundResult {pointsScored = 0, livesLost = (lifeLoss result)}
                else do
                  let newScore = score + (scoreResult result)
                  putStrLn ("Score for this round: " ++ (show newScore)) >> blankLine
                  loop newScore (rollNum + 1)
            else return $ RoundResult {pointsScored = score,
                                       livesLost    = 0}


endGame :: Int -> IO ()
endGame finalScore = do
  blankLine
  blankLine
  putStrLn $ "GAME OVER: Your final score is " ++ (show finalScore)

playAGame :: Int -> IO ()
playAGame nLives = do
  putStrLn "Welcome to Dodgy Dice!"
  loop nLives 0 1
    where loop lives score roundNum =
            if lives <= 0
            then endGame score
            else do
              blankLine >> (putStrLn $ "Round #" ++ (show roundNum)) >> blankLine
              printStatus lives score
              RoundResult ptsScored lvsLost <- playARound roundNum
              if lvsLost > 0
                then putStrLn ("You lost " ++ (show lvsLost) ++ " lives.")
                     >> loop (lives - lvsLost) score (roundNum + 1)
                else putStrLn ("You scored " ++ (show ptsScored) ++ " points.")
                     >> loop lives (score + ptsScored) (roundNum + 1)
          printStatus lives score =
            putStrLn $ "Lives left: " ++ (show lives) ++ " Score: " ++ (show score)

-- Exercise #4: Explain what's happening with this line, above:
--   (RoundResult ptsScored lvsLost <- playARound roundNum)

-- How would that language construct fail? Why is that not an issue in our
-- particular case? When that does fail, what happens?


-- Exercise #5: Currently, when you lose a life on rolling a 1, you see this at
-- the console:

--    "You lost 1 lives."

-- How would you change the code so that it says "You lost 1 life."?


-- Exercise #6: Define factorial at ghci like so.

-- ghci> let factorial :: Int -> Int; factorial n = foldl (*) 1 [1..n]

-- Explain why this formulation presents a type error:

-- ghci> factorial factorial 3

-- Fix it in two ways, one using parentheses and one using the `$` operator.


-- A Haskell executable, when run, executes an action called `main` with type
-- `IO ()`.

main :: IO ()
main = playAGame defaultNLives
