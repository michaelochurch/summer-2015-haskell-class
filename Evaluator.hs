import Types

oneStep :: LispState -> Lisp LispState
oneStep v@(Value _) = return v
oneStep (Apply lispFn lispValues) =
  case lispFn of
    (LFPrimitive _ apply) ->
      case apply lispValues of
        Left _ -> error "not implemented"
        Right v -> return $ Value v
oneStep f@(Form _) = error "not implemented"

dSum :: [Double] -> Double
dSum = sum

plus :: LispFunction
plus = liftFunction dSum "+"
