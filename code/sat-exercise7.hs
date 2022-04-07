module Exercise7 where
import Data.List (intercalate)

-- Define a data type for our expressions
data Expr = Const Bool
  | Var String
  | Not Expr
  | And [Expr]
  | Or [Expr]
  deriving Eq

-- Provide some custom pretty-printing
instance Show Expr where
  show (Const b) = show b
  show (Var a) = a
  show (Not e) = "¬" ++ show e ++ ""
  show (And es) = "(" ++ intercalate " ∧ " [show e | e <- es] ++ ")"
  show (Or es)  = "(" ++ intercalate " ∨ " [show e | e <- es] ++ ")"

-- Introduce Bindings as a synonym for list of (Char, Bool)
type Bindings = [(String, Bool)]

-- Look up a variable's value in some bindings
find :: String -> Bindings -> Bool
find = undefined

-- Evaluate an expression with variables bound to values to produce a boolean
eval :: Expr -> Bindings -> Bool
eval = undefined

-- Find the names of all free variables in an expression
vars :: Expr -> [String]
vars = undefined

-- Enumerate a list of all boolean assignments to lists of length n
bools :: Int -> [[Bool]]
bools = undefined

-- Remove duplicates from a list
uniquify :: Eq a => [a] -> [a]
uniquify [] = []
uniquify (x:xs) = x : filter (/= x) (uniquify xs)

-- Given an expression, produce a list of bindings
-- where each set of bindings is one of the possible assignments of
-- boolean values to the free variables in the expression
bindings :: Expr -> [Bindings]
bindings = undefined

-- Is an expression satisfiable? If yes, return Just the bindings that
-- satisfy it.
sat :: Expr -> Maybe Bindings
sat = undefined

-- Now with existential quantifiers

data QuantifiedExpr
  = Bare Expr
  | Forall String QuantifiedExpr
  | Exists String QuantifiedExpr
  deriving Eq

instance Show QuantifiedExpr where
  show (Exists c e) = "∃" ++ c ++ "." ++ show e
  show (Forall c e) = "∀" ++ c ++ "." ++ show e
  show (Bare e)     = show e

satQuantified :: QuantifiedExpr -> Maybe Bindings
satQuantified = sat . rewrite

-- We just need to implement this rewriting rule
-- forall a.expr(a) -> expr(True) ^ expr(False)
-- exists a.expr(a) -> expr(False) v expr(True)
rewrite :: QuantifiedExpr -> Expr
rewrite = undefined
