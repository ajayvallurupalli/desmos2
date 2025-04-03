module Expression where

import Prelude

import Data.Array (cons, foldl, head, reverse, splitAt, uncons, updateAt, (!!))
import Data.Array (length, take) as A
import Data.Either (Either(..), hush, note)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, keys, lookup)
import Data.Maybe (Maybe(..))
import Data.Number (sin)
import Data.Set (filter, toUnfoldable)
import Data.String (length, take)
import Data.String.CodeUnits (toCharArray, singleton)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Parse (cutChars, ParsePart(..))


type OperatorData =
  (precedence :: Int, parenthesisLevel :: Int, name :: String)

type VariableData = 
  {parenthesisLevel :: Int, symbol :: String}

type Error = String -- I think it helps with making code more readable

type Result = Either Error Expression

type BinaryOperator = {
  op :: (Expression -> Expression -> Result),
  infix :: Boolean
  | OperatorData
}

type UnaryOperator = {
  op :: (Expression -> Result)
  | OperatorData
}

transformBinary :: (Number -> Number -> Number) -> String -> (Expression -> Expression -> Result)
transformBinary f _ (Value x) (Value y) = pure $ Value $ f x y
transformBinary _ n (Operator _) (Operator _) = Left $ "Error: Cannot " <> n <> " functions."
transformBinary _ n (Value _) (Operator _) = Left $ "Error: Cannot " <> n <> " a value with a function."
transformBinary _ n (Operator _) (Value _) = Left $ "Error: Cannot " <> n <> " a value with a function."
transformBinary _ n _ _ = Left $ "Error: Invalid aruments for " <> n <> "."

transformUnary :: (Number -> Number) -> String -> (Expression -> Result)
transformUnary f _ (Value x) = pure $ Value $ f x
transformUnary _ n (Operator _) = Left $ "Error: Cannot " <> n <> " a function."
transformUnary _ n _ = Left $ "Error: Invalid aruments for " <> n <> "."

mulop :: Operator 
mulop = Binary $ {op: transformBinary mul "multiply", precedence: 10, parenthesisLevel: 0, infix: true, name: "*"}

addop :: Operator 
addop = Binary $ {op: transformBinary add "add", precedence: 6, parenthesisLevel: 0, infix: true, name: "*"}

sinop :: Operator 
sinop = Unary $ {op: transformUnary sin "sin", precedence: 20, parenthesisLevel: 0, name: "sin"}

data Operator 
  = Unary UnaryOperator
  | Binary BinaryOperator

instance eqOperator :: Eq Operator where
  eq (Binary b1) (Binary b2) = b1.name == b2.name && b1.parenthesisLevel == b2.parenthesisLevel
  eq (Unary u1) (Unary u2) = u1.name == u2.name && u1.parenthesisLevel == u2.parenthesisLevel
  eq _ _ = false

data Expression 
  = Value Number
  | Operator Operator
  | Variable VariableData

instance eqExpression :: Eq Expression where
  eq (Value v1) (Value v2) = v1 == v2
  eq (Operator o1) (Operator o2) = o1 == o2
  eq (Variable v1) (Variable v2) = v1 == v2
  eq _ _ = false

showOperator :: ∀ r. String -> {name :: String, precedence :: Int, parenthesisLevel :: Int | r} -> String
showOperator s o =
  "(Exp. " <> 
  s <> " Operator: " <> o.name <> 
  " with precedence of " <> show o.precedence <> 
  " and parenthesis level of " <> show o.parenthesisLevel <> ")"

instance showExpression :: Show Expression where
  show t = 
    case t of   
      Value v -> "(Exp. Value: " <> show v <> ")"
      Operator (Binary b) -> if b.infix then showOperator "Infixed Binary" b else showOperator "Binary" b
      Operator (Unary u) -> showOperator "Unary" u
      Variable v -> "(Exp. Variable: " <> v.symbol <> ")"

variable :: String -> Expression
variable s = Variable {parenthesisLevel: 0, symbol: s}

type SymbolMap = (Map String Expression)

type State = {
  parenthesis :: Int,
  result :: Either Error (Array Expression)
}

emptyState :: State
emptyState = {parenthesis: 0, result: Right []}

prefixEqual :: String -> String -> Boolean
prefixEqual p s = p == (take (length p) s)

possibleStrings :: SymbolMap -> String -> (Array String) 
possibleStrings m s = toUnfoldable $ filter (prefixEqual s) (keys m)

--yeah idk how this works but it does
parseSymbols :: SymbolMap -> String -> Either Error (Array Expression)
parseSymbols m s = 
  let 
    error = "Error: '" <> s <> "' is not a binded variable or function."  
    aux acc sacc cs =
      case uncons cs of 
        Nothing -> if sacc == "" then acc else cons <$> (note error (lookup sacc m)) <*> acc
        Just {head, tail} -> 
          if 0 == A.length (possibleStrings m (sacc <> singleton head)) then 
            if sacc == "" then Left $ error else 
              aux (cons <$> (note "This Error shouldn't be possible" (lookup sacc m)) <*> acc) "" cs --the Value 0.0 is an error
          else 
            aux acc (sacc <> singleton head) tail
  in 
  aux (pure []) "" (toCharArray s)

-- idk if theres a way to not need to type every one. Theres no generic constructor / pattern match constructor
parenthesize :: Int -> Expression -> Expression
parenthesize pl (Operator (Binary b)) = Operator $ Binary $ b {parenthesisLevel = b.parenthesisLevel + pl}
parenthesize pl (Operator (Unary u)) = Operator $  Unary $ u {parenthesisLevel = u.parenthesisLevel + pl}
parenthesize pl (Variable v) = Variable $ v {parenthesisLevel = v.parenthesisLevel + pl}
parenthesize _ other = other

parse :: SymbolMap -> String -> Either Error (Array Expression)
parse m s = do
    l <- cutChars s
    let r = foldl aux emptyState l
    result <- r.result
    if r.parenthesis /= 0 then Left "Error: Unequal number of parenthesis."
    else pure $ reverse result
  where
    aux :: State -> ParsePart -> State
    aux acc pp = 
      case pp of 
        Digit d -> acc {result = cons (Value d) <$> acc.result}
        Parenthesis p -> 
          case hush acc.result >>= head of
            Just (Value _) | p > 0 -> acc {parenthesis = acc.parenthesis + p, result = cons (Operator mulop) <$> acc.result}
            Just (Variable _) | p > 0 -> acc {parenthesis = acc.parenthesis + p, result = cons (Operator mulop) <$> acc.result}
            _ -> acc {parenthesis = acc.parenthesis + p}
        Letter ls -> acc {result = append <$> (map (parenthesize acc.parenthesis) <$> (parseSymbols m ls)) <*> acc.result}

fillVariable :: SymbolMap -> Expression -> Either Error Expression
fillVariable m (Variable s) = 
  case lookup s.symbol m of
    Nothing -> Left $ "Error: Variable " <> s.symbol <> " does not exist"
    Just (Variable s2) -> fillVariable m (Variable s2)
    Just x -> pure (parenthesize s.parenthesisLevel x)
fillVariable _ other = pure other

fillVariables :: SymbolMap -> Array Expression -> Either Error (Array Expression)
fillVariables m es = sequence (map (fillVariable m) es)

deleteExtraMultiplies :: Array Expression -> (Array Expression)
deleteExtraMultiplies es = aux [] es
  where
    aux acc [] = acc
    aux acc dacc = 
      let s = splitAt 2 dacc in
      case s.before of 
        [Operator x, Operator m] | m == mulop -> aux (append acc [Operator x]) s.after
        other -> aux (append acc other) s.after

lispify :: ∀ a. Error -> Error -> Array a -> Int -> Either Error (Array a)
lispify e1 e2 a i = do
    ati <- note e1 (a !! i)
    atbi <- note e2 (a !! (i - 1))
    fs <- note e1 (updateAt i atbi a) -- these errors shouldn't be possible
    note e2 (updateAt (i - 1) ati fs)

power :: Operator -> Int
power (Binary {parenthesisLevel, precedence}) = parenthesisLevel * 100 + precedence 
power (Unary {parenthesisLevel, precedence}) = parenthesisLevel * 100 + precedence

maxPower :: Array Expression -> Int
maxPower = fst <<< foldlWithIndex aux (Tuple (-1) (-1))
  where
    aux i acc e = 
      case e of
        (Operator o) | (snd acc) < (power o) -> Tuple i (power o)
        _ -> acc

isInfix :: Expression -> Boolean
isInfix (Operator (Binary b)) = b.infix
isInfix _ = false

runUnary :: UnaryOperator -> Array Expression -> Either Error (Expression)
runUnary u [x1] = u.op x1
runUnary _ _ = Left "Error: Invalid Number of Arguments"

runBinary :: BinaryOperator -> Array Expression -> Either Error (Expression)
runBinary b [x1, x2] = b.op x1 x2 
runBinary _ _ = Left "Error: Invalid Number of Arguments"

runOperator :: Expression -> Array Expression -> Either Error (Tuple (Array Expression) Expression)
runOperator (Operator (Unary u)) es = let s = splitAt 1 es in sequence $ Tuple s.after (runUnary u s.before)
runOperator (Operator (Binary b)) es = let s = splitAt 2 es in sequence $ Tuple s.after (runBinary b s.before)
runOperator _ _ = Left "Error: ???"

runExpressions :: Array Expression -> SymbolMap -> Either Error Number
runExpressions e vm = do
  esv <- fillVariables vm e
  aux (deleteExtraMultiplies esv)
  where
    ierror = "Error: This error should be impossible" 
    argError = "Error: Invalid Number of Arguments"
    aux [Value v] = pure v
    aux es = do
      let i = maxPower es 
      o <- note ierror (es !! i)
      nes <- if isInfix o then lispify argError ierror es i else pure es
      let ni = if isInfix o then i else i + 1
      let snes = splitAt ni nes
      res <- runOperator o snes.after
      rres <- case snd res of
        (Variable s) -> fillVariable vm (Variable s)
        _ -> pure $ snd res
      let nxt = append (A.take (A.length snes.before - 1) snes.before) (cons rres (fst res))
      aux nxt


