module Exercise07 where
import Data.List (intercalate)
import Data.Maybe
-- This is like "import Data.Set as Set" in python, so that everything
-- from the package is available as Set.xxx
import qualified Data.Set as Set

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
-- Note that this function is not total, it errors if the requested
-- name does not exist in the bindings.
find :: String -> Bindings -> Bool
find name bs = head [v | (k, v) <- bs, k == name]

-- Evaluate an expression with variables bound to values to produce a
-- boolean
-- We just have to translate the Expression nodes to their obvious
-- Boolean counterpart, recursing on the contents.
eval :: Expr -> Bindings -> Bool
eval (Const b) _ = b
eval (Var x) bs = find x bs
eval (Not e) bs =  not $ eval e bs
eval (And es) bs = and [eval e bs | e <- es]
eval (Or es) bs = or [eval e bs | e <- es]

-- Find the names of all free variables in an expression
-- Recurse, concatenating the results
vars :: Expr -> [String]
vars (Const _) = []
vars (Var x) = [x]
vars (Not e) = vars e
vars (And es) = concatMap vars es
vars (Or es) = concatMap vars es

-- Enumerate a list of all boolean assignments to lists of length n
-- The idea here is that the only list of boolean assignments we can
-- have for zero assignments is []
-- For the recursion, we cons False onto the result of recursing and
-- separately True, and concatenate those two lists.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n - 1)

-- Remove duplicates from a list
-- This is quadratic in the list length, but maintains order.
-- If you didn't want to maintain order you could sort and compress
-- (for nlog n). Most Haskell implementations of Set-like data
-- structures, e.g. Data.Set
-- (https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Set.html)
-- use tree-like data structures under the hood, so construction is
-- nlog n too.
uniquify :: Eq a => [a] -> [a]
uniquify [] = []
uniquify (x:xs) = x : filter (/= x) (uniquify xs)

-- For example, uniquifying without caring about order
-- the Data.Set implementation requires an ordering on the elements
-- for efficiency purposes see this
-- https://stackoverflow.com/a/28660652 stackoverflow post for explanation
uniquifyUnordered :: Ord a => [a] -> [a]
uniquifyUnordered xs = Set.toList (Set.fromList xs)

-- Given an expression, produce a list of bindings
-- where each set of bindings is one of the possible assignments of
-- boolean values to the free variables in the expression
bindings :: Expr -> [Bindings]
bindings e = map (zip vs) (bools (length vs))
  where vs = uniquify (vars e)


-- Here's a helper function, it takes lists of of bindings and checks
-- if any combination satisfys the expression
sat' :: Expr -> [Bindings] -> Maybe Bindings
-- If the expression is a constant boolean, Just [] satisfies it if it
-- is True, and Nothing satisfies it if it is False.
sat' (Const b) _ | b = Just []
                 | otherwise = Nothing
-- We have no more bindings to try, but our expression is not a Const
-- object, so we can't satisfy the expression.
sat' _ [] = Nothing
-- We have at least one set of bindings to try and a non-Const
-- expression
-- So try it, if the expression evaluates to True, we are done, and
-- return Just bs, otherwise recurse on the rest of the bindings.
sat' e (bs:bss) = let expr = eval e bs in
                  if expr 
                  then Just bs
                  else sat' e bss

expr = Or [And [Var "a", Var "b"], Not (Var "c")]


-- Is an expression satisfiable? If yes, return Just the bindings that
-- satisfy it.
sat :: Expr -> Maybe Bindings
sat e = sat' e $ bindings e

-- Here we introduce our new data type for quantified expressions
-- These are either a bare expression (unquantified) or else a
-- quantification. We need Bare Expr rather than just Expr, because
-- Expr names a type which we will want to unwrap
data QuantifiedExpr
  = Bare Expr
  | Forall String QuantifiedExpr
  | Exists String QuantifiedExpr
  deriving Eq

-- Custom pretty-printing.
instance Show QuantifiedExpr where
  show (Exists c e) = "∃" ++ c ++ "." ++ show e
  show (Forall c e) = "∀" ++ c ++ "." ++ show e
  show (Bare e)     = show e

-- Our approach is to rewrite a QuantifiedExpr into a normal Expr
-- using the rewrite rules
-- ∃ a. expr(a) -> expr(True) ∨ expr(False)
-- ∀ a. expr(a) -> expr(True) ∧ expr(False)
-- We do it with the use of a helper function that maintains the
-- bindings from quantified variables to values.
rewrite :: QuantifiedExpr -> Expr
rewrite = rewrite' []

-- Substitute any variables in an expression with values given by the
-- binding. If the variable doesn't exist in the bindings, then just
-- return it.
subst :: Bindings -> Expr -> Expr
-- ASIDE:
-- This definition is an irritation: because our find function errors if the
-- variable is not found in the bindings, we cannot use it here (since
-- it might error). So we rewrite and only access the head if we need
-- to.
-- A better approach would be for find to have type
-- find :: String -> Bindings -> Maybe Bool
-- which would require some refactoring of the rest of the code. In
-- particular, the one place where we notice this is slightly
-- difficult is that we end up needing a function andM :: [Maybe Bool]
-- -> Maybe Bool that is like and lifted to act on Maybes. This is
-- available in the Control.Monad.Extra package
-- (https://hackage.haskell.org/package/extra)
-- END ASIDE.
subst bs var@(Var a) = let found = [v | (k, v) <- bs, k == a] in
                         if null found
                         then var
                         else Const (head found)
-- The rest is just recursion.
subst _ (Const b) = Const b
subst bs (Not e) = Not (subst bs e)
subst bs (Or es) = Or $ map (subst bs) es
subst bs (And es) = And $ map (subst bs) es

rewrite' :: Bindings -> QuantifiedExpr -> Expr
-- Just unwrap the Bare expression and substitute.
rewrite' bs (Bare e) = subst bs e
-- Rewrite rules for Exists and Forall
rewrite' bs (Exists c e) = Or [rewrite' ((c,v):bs) e | v <- [True, False]]
rewrite' bs (Forall c e) = And [rewrite' ((c,v):bs) e | v <- [True, False]]

-- And this is now straightforward
satQuantified :: QuantifiedExpr -> Maybe Bindings
satQuantified = sat . rewrite
