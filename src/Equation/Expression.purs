module Equation.Expression
  ( AnyOperator
  , BinaryOperator
  , Error
  , Expression(..)
  , GenereralData
  , Operator(..)
  , OperatorData
  , Result
  , SymbolMap(..)
  , UnaryOperator
  , ValueData
  , VariableData
  , VariableMap(..)
  , VariableType(..)
  , emptyState
  , fromExpression
  , getOperatorDependencies
  , getVariableDependencies
  , mulop
  , opName
  , parenthesisLevelOf
  , parse
  , parseSymbols'
  , runExpressions
  , transformBinary
  , transformBinary'
  , transformUnary
  , transformUnary'
  , unionSymbols
  , unionVariables
  , value
  , variable
  )
  where

import Prelude

import Data.Array (cons, foldl, head, reverse, splitAt, uncons, updateAt, (!!))
import Data.Array (filter, length, take) as A
import Data.Either (Either(..), hush, note)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, keys, lookup, union)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over2, unwrap)
import Data.Set (filter, toUnfoldable)
import Data.Show.Generic (genericShow)
import Data.String (length, take)
import Data.String.CodeUnits (toCharArray, singleton)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Equation.Parse (cutChars, ParsePart(..))


type GenereralData = (parenthesisLevel :: Int)

type OperatorData =
  (precedence :: Int, name :: String | GenereralData)

type VariableData = 
  {symbol :: String, type' :: VariableType | GenereralData}

type ValueData = 
  {num :: Number | GenereralData}

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

type AnyOperator = {
  op :: (Array Expression -> Result),
  n :: Int
  | OperatorData
}

updateNum :: ValueData -> Number -> Expression
updateNum x = ((x {num = _}) >>> Value)

transformBinary :: (Number -> Number -> Number) -> String -> (Expression -> Expression -> Result)
transformBinary f _ (Value x) (Value y) = pure $ updateNum x $ f x.num y.num
transformBinary _ n (Operator _) (Operator _) = Left $ "Error: Cannot " <> n <> " functions."
transformBinary _ n (Value _) (Operator _) = Left $ "Error: Cannot " <> n <> " a value with a function."
transformBinary _ n (Operator _) (Value _) = Left $ "Error: Cannot " <> n <> " a value with a function."
transformBinary _ n _ _ = Left $ "Error: Invalid aruments for " <> n <> "."

--for when answere can error
transformBinary' :: (Number -> Number -> Either Error Number) -> String -> (Expression -> Expression -> Result)
transformBinary' f _ (Value x) (Value y) = updateNum x <$> f x.num y.num
transformBinary' _ n (Operator _) (Operator _) = Left $ "Error: Cannot " <> n <> " functions."
transformBinary' _ n (Value _) (Operator _) = Left $ "Error: Cannot " <> n <> " a value with a function."
transformBinary' _ n (Operator _) (Value _) = Left $ "Error: Cannot " <> n <> " a value with a function."
transformBinary' _ n _ _ = Left $ "Error: Invalid aruments for " <> n <> "."

transformUnary :: (Number -> Number) -> String -> (Expression -> Result)
transformUnary f _ (Value x) = pure $ updateNum x $ f x.num 
transformUnary _ n (Operator _) = Left $ "Error: Cannot " <> n <> " a function."
transformUnary _ n _ = Left $ "Error: Invalid aruments for " <> n <> "."

--for when answer can error
transformUnary' :: (Number -> Either Error Number) -> String -> (Expression -> Result)
transformUnary' f _ (Value x) = updateNum x <$> f x.num  
transformUnary' _ n (Operator _) = Left $ "Error: Cannot " <> n <> " a function."
transformUnary' _ n _ = Left $ "Error: Invalid aruments for " <> n <> "."

mulop :: Operator 
mulop = Binary $ {op: transformBinary mul "multiply", precedence: 10, parenthesisLevel: 0, infix: true, name: "*"}

data Operator 
  = Unary UnaryOperator
  | Binary BinaryOperator
  | Any AnyOperator

opName :: Operator -> String
opName (Binary b) = b.name
opName (Unary u) = u.name
opName (Any a) = a.name

instance eqOperator :: Eq Operator where
  eq (Binary b1) (Binary b2) = b1.name == b2.name
  eq (Unary u1) (Unary u2) = u1.name == u2.name
  eq _ _ = false

data Expression 
  = Value ValueData
  | Operator Operator
  | Variable VariableData

data VariableType 
  = Value'
  | Operator'

derive instance eqVariableType :: Eq VariableType

instance eqExpression :: Eq Expression where
  eq (Value v1) (Value v2) = v1.num == v2.num
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
      Value v -> "(Exp. Value: " <> show v.num <> ")"
      Operator (Binary b) -> if b.infix then showOperator "Infixed Binary" b else showOperator "Binary" b
      Operator (Unary u) -> showOperator "Unary" u
      Operator (Any a) -> showOperator (show a.n <> "-arg") a
      Variable v -> "(Exp. Variable: " <> v.symbol <> ")"

-- inverse of the whole parsing thing
fromExpression :: Expression -> String
fromExpression (Value v) = show v.num
fromExpression (Operator o) = opName o 
fromExpression (Variable v) = v.symbol

parenthesisLevelOf :: Expression -> Int
parenthesisLevelOf (Operator (Binary b)) = b.parenthesisLevel
parenthesisLevelOf (Operator (Unary u)) = u.parenthesisLevel
parenthesisLevelOf (Operator (Any a)) = a.parenthesisLevel
parenthesisLevelOf (Variable v) = v.parenthesisLevel
parenthesisLevelOf (Value v) = v.parenthesisLevel

variable :: String -> VariableType -> Expression
variable s vt = Variable {parenthesisLevel: 0, symbol: s, type': vt}

value :: Number -> Expression
value n = Value {parenthesisLevel: 0, num: n}

-- Seperating Variables and Symbols to avoid mixing
newtype SymbolMap = SymbolMap (Map String Expression)
derive instance newtypeSymbolMap :: Newtype SymbolMap _
derive instance genericSymbolMap :: Generic SymbolMap _ 
instance showSymbolMap :: Show SymbolMap where
  show = genericShow

--SymbolMap is of kind type, so it doesn't really fit with alt
-- but it also doesn't match with semigroup because a `union` b != b `union` a
-- so i guess no type classes
unionSymbols :: SymbolMap -> SymbolMap -> SymbolMap
unionSymbols = over2 SymbolMap union 

newtype VariableMap = VariableMap (Map String Expression)
derive instance newtypeVariableMap :: Newtype VariableMap _
derive instance genericVariableMap :: Generic VariableMap _ 
instance showVariableMap :: Show VariableMap where
  show = genericShow

unionVariables :: VariableMap -> VariableMap -> VariableMap
unionVariables = over2 VariableMap union 

prefixEqual :: String -> String -> Boolean
prefixEqual p s = p == (take (length p) s)

possibleStrings :: SymbolMap -> String -> (Array String) 
possibleStrings m s = toUnfoldable $ filter (prefixEqual s) (keys (unwrap m))

parseSymbols :: SymbolMap -> String -> Either Error (Array Expression)
parseSymbols sm@(SymbolMap m) s = 
  let 
    error = "Error: '" <> s <> "' is not a binded variable or function."  
    aux :: Either Error (Array Expression) -> String -> Array Char -> Either Error (Array Expression)
    aux acc sacc cs =
      case uncons cs of 
        Nothing -> if sacc == "" then acc else cons <$> (note error (lookup sacc m)) <*> acc
        Just {head, tail} -> 
          if 0 == A.length (possibleStrings sm (sacc <> singleton head)) then 
            if sacc == "" then Left $ error else 
              aux (cons <$> (note "This Error shouldn't be possible" (lookup sacc m)) <*> acc) "" cs
          else 
            aux acc (sacc <> singleton head) tail
  in 
  aux (pure []) "" (toCharArray s)

parseSymbols' :: SymbolMap -> String -> Either Error (Array String)
parseSymbols' sm s = 
  let 
    error = "Error: '" <> s <> "' is not a binded variable or function."  
    aux acc sacc cs =
      case uncons cs of 
        Nothing -> if sacc == "" then acc else cons sacc <$> acc
        Just {head, tail} -> 
          if 0 == A.length (possibleStrings sm (sacc <> singleton head)) then 
            if sacc == "" then Left $ error else 
              aux (cons sacc <$> acc) "" cs
          else 
            aux acc (sacc <> singleton head) tail
  in 
  aux (pure []) "" (toCharArray s)

-- idk if theres a way to not need to type every one. Theres no generic constructor / pattern match constructor
parenthesize :: Int -> Expression -> Expression
parenthesize pl (Operator (Binary b)) = Operator $ Binary $ b {parenthesisLevel = b.parenthesisLevel + pl}
parenthesize pl (Operator (Unary u))  = Operator $ Unary $ u {parenthesisLevel = u.parenthesisLevel + pl}
parenthesize pl (Operator (Any u))  = Operator $ Any $ u {parenthesisLevel = u.parenthesisLevel + pl}
parenthesize pl (Variable v) = Variable $ v {parenthesisLevel = v.parenthesisLevel + pl}
parenthesize pl (Value v) = Value $ v {parenthesisLevel = v.parenthesisLevel + pl}

type State = {
  parenthesis :: Int,
  result :: Either Error (Array Expression),
  comma :: Boolean
}

emptyState :: State
emptyState = {parenthesis: 0, result: Right [], comma: false}

processVariables :: Array Expression -> Array Expression 
processVariables es = aux [] es
  where
    aux acc [] = acc
    aux acc dacc = 
      let s = splitAt 2 dacc in
      case s.before of 
        [v1@(Variable {type': Value'}), v2@(Variable {type': Value'})] -> aux (append acc [v1, (Operator mulop)]) (cons v2 s.after)
        [v1@(Variable {type': Value'}), v2@(Value _)] -> aux (append acc [v1, (Operator mulop)]) (cons v2 s.after)
        [v1@(Value _), v2@(Variable {type': Value'})] -> aux (append acc [v1, (Operator mulop)]) (cons v2 s.after)
        [v1@(Value _), v2@(Value _)] -> aux (append acc [v1, (Operator mulop)]) (cons v2 s.after)
        other -> aux (append acc other) s.after

parse :: SymbolMap -> String -> Either Error (Array Expression)
parse m s = do
    l <- cutChars s
    let r = foldl aux emptyState l
    result <- r.result
    if r.parenthesis /= 0 then Left "Error: Unequal number of parenthesis."
    else pure $ reverse result
  where
    varOrVal (Value _) = true 
    varOrVal (Variable {type': Value'}) = true
    varOrVal _ = false
    aux :: State -> ParsePart -> State
    aux acc pp = 
      case pp of 
        Digit d -> acc {result = cons (parenthesize acc.parenthesis $ value d) <$> acc.result, comma = false}
        Parenthesis p -> 
          case hush acc.result >>= head of
            Just v1 | p > 0 && varOrVal v1 -> acc {parenthesis = acc.parenthesis + p, result = cons (Operator mulop) <$> acc.result, comma = false}
            _ -> acc {parenthesis = acc.parenthesis + p, comma = false}
        Letter ls -> 
          let symbols = (map (parenthesize acc.parenthesis) <$> processVariables <$> (parseSymbols m ls)) in
          let noMulResult = append <$> symbols <*> acc.result in
          if not acc.comma then
            case (hush acc.result >>= head), (hush symbols >>= head) of 
              Just v1, Just v2 | varOrVal v1 && varOrVal v2-> acc {result = append <$> symbols <*> (cons (Operator mulop) <$> acc.result), comma = false}
              _, _ -> acc {result = noMulResult, comma = false}
          else 
            acc {result = noMulResult, comma = false}
        Comma -> acc {comma = true}

fillVariable :: VariableMap -> Expression -> Either Error Expression
fillVariable m (Variable s) = 
  case lookup s.symbol (unwrap m) of
    Nothing -> Left $ "Error: Variable " <> s.symbol <> " does not exist"
    Just v2@(Variable _) -> fillVariable m v2
    Just x -> pure (parenthesize s.parenthesisLevel x)
fillVariable _ other = pure other

fillVariables :: VariableMap -> Array Expression -> Either Error (Array Expression)
fillVariables m es = sequence (map (fillVariable m) es)

lispify :: ∀ a. Error -> Error -> Array a -> Int -> Either Error (Array a)
lispify e1 e2 a i = do
    ati <- note e1 (a !! i)
    atbi <- note e2 (a !! (i - 1))
    fs <- note e1 (updateAt i atbi a) -- these errors shouldn't be possible
    note e2 (updateAt (i - 1) ati fs)

power :: Operator -> Int
power (Binary {parenthesisLevel, precedence}) = parenthesisLevel * 100 + precedence 
power (Unary {parenthesisLevel, precedence}) = parenthesisLevel * 100 + precedence
power (Any {parenthesisLevel, precedence}) = parenthesisLevel * 100 + precedence

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

-- I should probably do UnaryOperator -> Array Expression -> Either Error Expression 
--instead of cheating with array expression, but its easier 
runUnary :: UnaryOperator -> Array Expression -> Either Error Expression
runUnary u [x1] = u.op x1
runUnary _ _ = Left "Error: Invalid Number of Arguments"

runBinary :: BinaryOperator -> Array Expression -> Either Error Expression
runBinary b [x1, x2] = b.op x1 x2 
runBinary _ _ = Left "Error: Invalid Number of Arguments"

runAnyOp :: AnyOperator -> Array Expression -> Either Error Expression 
runAnyOp a ls = if A.length ls /= a.n then Left "Error: Invalid Number of Arguments" else a.op ls

runOperator :: Expression -> Array Expression -> Either Error (Tuple (Array Expression) Expression)
runOperator (Operator (Unary u)) es = let s = splitAt 1 es in sequence $ Tuple s.after (runUnary u s.before)
runOperator (Operator (Binary b)) es = let s = splitAt 2 es in sequence $ Tuple s.after (runBinary b s.before)
runOperator (Operator (Any a)) es = let s = splitAt a.n es in sequence $ Tuple s.after (runAnyOp a s.before)
runOperator _ _ = Left "Error: ???"

cutOperator :: Expression -> Array Expression -> Tuple (Array Expression) Expression
cutOperator (Operator (Unary _)) es = let s = splitAt 1 es in Tuple s.after (value 1.0)
cutOperator (Operator (Binary _)) es = let s = splitAt 2 es in Tuple s.after (value 1.0)
cutOperator (Operator (Any a)) es = let s = splitAt a.n es in Tuple s.after (value 1.0)
cutOperator _ _ = Tuple [] (value 1.0) --should be impossible..?

runExpressions :: Array Expression -> VariableMap -> Either Error Number
runExpressions e vm = do
  esv <- fillVariables vm e
  aux esv
  where
    ierror = "Error: This error should be impossible" 
    argError = "Error: Invalid Number of Arguments"
    aux [Value v] = pure v.num
    aux es = do
      let i = maxPower es 
      o <- note ierror (es !! i)
      nes <- if isInfix o then lispify argError ierror es i else pure es
      let ni = if isInfix o then i else i + 1
      let snes = splitAt ni nes
      res <- runOperator o snes.after
      rres <- case snd res of
        var@(Variable _) -> fillVariable vm var
        _ -> pure $ snd res
      let nxt = append (A.take (A.length snes.before - 1) snes.before) (cons rres (fst res))
      aux nxt

--this one just pretends to do the math to find the dependencies
--t(technicaly doesnt find all of them, because of functions that could return operators)
--but I don't thinkthat's too important
--and besides the only real way to solve that would be to make every operator list potential returns
--or conduct analysis on all objects
getOperatorDependencies :: Array Expression -> Either Error (Array Expression)
getOperatorDependencies e = do
  aux [] e
  where
    ierror = "Error: This error should be impossible" 
    argError = "Error: Invalid Number of Arguments"
    aux acc [Value _] = pure acc
    aux acc es = do
      let i = maxPower es 
      o <- note ierror (es !! i)
      nes <- if isInfix o then lispify argError ierror es i else pure es
      let ni = if isInfix o then i else i + 1
      let snes = splitAt ni nes
      let res = cutOperator o snes.after
      let rres = snd res
      let nxt = append (A.take (A.length snes.before - 1) snes.before) (cons rres (fst res))
      aux (cons o acc) nxt

getVariableDependencies :: Array Expression -> Array Expression 
getVariableDependencies = A.filter isVar
  where
    isVar (Variable _) = true
    isVar _ = false