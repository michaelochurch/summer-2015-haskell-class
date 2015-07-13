module Builtins where

import qualified Data.Map as M
import qualified Data.Set as S
import Types

dSum :: [Double] -> Double
dSum = sum

plus :: LispFunction
plus = liftFunction dSum "+"

-- dNumEq :: [Double] -> Double
-- dNumEq [d1, d2] = 

globalBuiltins :: M.Map String LispValue
globalBuiltins = M.fromList [("+", LVFunction plus),
                             ("pi", LVNumber pi)]

initEnv :: LispEnv
initEnv = LispEnv [] globalBuiltins 1 S.empty
