module Expression
  ( BinaryOperator
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
  , emptyState
  , fromExpression
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
import Data.Array (length, take) as A
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
import Parse (cutChars, ParsePart(..))


type GenereralData = (parenthesisLevel :: Int)

type OperatorData =
  (precedence :: Int, name :: String | GenereralData)

type VariableData = 
  {symbol :: String | GenereralData}

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

opName :: Operator -> String
opName (Binary b) = b.name
opName (Unary u) = u.name

instance eqOperator :: Eq Operator where
  eq (Binary b1) (Binary b2) = b1.name == b2.name
  eq (Unary u1) (Unary u2) = u1.name == u2.name
  eq _ _ = false

data Expression 
  = Value ValueData
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
      Value v -> "(Exp. Value: " <> show v.num <> ")"
      Operator (Binary b) -> if b.infix then showOperator "Infixed Binary" b else showOperator "Binary" b
      Operator (Unary u) -> showOperator "Unary" u
      Variable v -> "(Exp. Variable: " <> v.symbol <> ")"

-- inverse of the whole parsing thing
fromExpression :: Expression -> String
fromExpression (Value v) = show v.num
fromExpression (Operator o) = opName o 
fromExpression (Variable v) = v.symbol

parenthesisLevelOf :: Expression -> Int
parenthesisLevelOf (Operator (Binary b)) = b.parenthesisLevel
parenthesisLevelOf (Operator (Unary u)) = u.parenthesisLevel
parenthesisLevelOf (Variable v) = v.parenthesisLevel
parenthesisLevelOf (Value v) = v.parenthesisLevel

variable :: String -> Expression
variable s = Variable {parenthesisLevel: 0, symbol: s}

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

type State = {
  parenthesis :: Int,
  result :: Either Error (Array Expression)
}

emptyState :: State
emptyState = {parenthesis: 0, result: Right []}

prefixEqual :: String -> String -> Boolean
prefixEqual p s = p == (take (length p) s)

possibleStrings :: SymbolMap -> String -> (Array String) 
possibleStrings m s = toUnfoldable $ filter (prefixEqual s) (keys (unwrap m))

--yeah idk how this works but it does
parseSymbols :: SymbolMap -> String -> Either Error (Array Expression)
parseSymbols sm@(SymbolMap m) s = 
  let 
    error = "Error: '" <> s <> "' is not a binded variable or function."  
    aux acc sacc cs =
      case uncons cs of 
        Nothing -> if sacc == "" then acc else cons <$> (note error (lookup sacc m)) <*> acc
        Just {head, tail} -> 
          if 0 == A.length (possibleStrings sm (sacc <> singleton head)) then 
            if sacc == "" then Left $ error else 
              aux (cons <$> (note "This Error shouldn't be possible" (lookup sacc m)) <*> acc) "" cs --the Value 0.0 is an error
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
              aux (cons sacc <$> acc) "" cs --the Value 0.0 is an error
          else 
            aux acc (sacc <> singleton head) tail
  in 
  aux (pure []) "" (toCharArray s)

-- idk if theres a way to not need to type every one. Theres no generic constructor / pattern match constructor
parenthesize :: Int -> Expression -> Expression
parenthesize pl (Operator (Binary b)) = Operator $ Binary $ b {parenthesisLevel = b.parenthesisLevel + pl}
parenthesize pl (Operator (Unary u)) = Operator $  Unary $ u {parenthesisLevel = u.parenthesisLevel + pl}
parenthesize pl (Variable v) = Variable $ v {parenthesisLevel = v.parenthesisLevel + pl}
parenthesize pl (Value v) = Value $ v {parenthesisLevel = v.parenthesisLevel + pl}

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
        Digit d -> acc {result = cons (parenthesize acc.parenthesis $ value d) <$> acc.result}
        Parenthesis p -> 
          case hush acc.result >>= head of
            Just (Value _) | p > 0 -> acc {parenthesis = acc.parenthesis + p, result = cons (Operator mulop) <$> acc.result}
            Just (Variable _) | p > 0 -> acc {parenthesis = acc.parenthesis + p, result = cons (Operator mulop) <$> acc.result}
            _ -> acc {parenthesis = acc.parenthesis + p}
        Letter ls -> acc {result = append <$> (map (parenthesize acc.parenthesis) <$> (parseSymbols m ls)) <*> acc.result}

fillVariable :: VariableMap -> Expression -> Either Error Expression
fillVariable m (Variable s) = 
  case lookup s.symbol (unwrap m) of
    Nothing -> Left $ "Error: Variable " <> s.symbol <> " does not exist"
    Just v2@(Variable _) -> fillVariable m v2
    Just x -> pure (parenthesize s.parenthesisLevel x)
fillVariable _ other = pure other

fillVariables :: VariableMap -> Array Expression -> Either Error (Array Expression)
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

runExpressions :: Array Expression -> VariableMap -> Either Error Number
runExpressions e vm = do
  esv <- fillVariables vm e
  aux (deleteExtraMultiplies esv)
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