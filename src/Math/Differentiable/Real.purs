module Math.Differentiable.Real
  ( evalExpr
  , derivative
  , ddx
  , nthDerivative
  , taylorExpansion
  ) where

import Prelude
import Math.Differentiable.Class

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), take)
import Data.Foldable (foldr)
import Data.Unfoldable (unfoldr)
import qualified Data.Int as Int

import Control.Monad.Error.Class (throwError)
import Math (pow, log, exp, sin, cos)

simplify :: forall a. (Realizable a) => Expr a -> Expr Number
simplify = simplify' <<< map toReal

simplify' :: Expr Number -> Expr Number
simplify' (Plus (Const x) (Const y)) = Const (toReal x + toReal y)
simplify' (Plus x (Const 0.0)) = simplify' x
simplify' (Plus (Const 0.0) y) = simplify' y
simplify' (Plus (Div (Const c1) x) (Div (Const c2) y)) =
  (((Const c1) * y) + ((Const c2) * x)) :/: (x * y)

simplify' (Mult (Const x) (Const y)) = Const (x * y)
simplify' (Mult x (Const 0.0)) = zero
simplify' (Mult x (Const 1.0)) = simplify' x
simplify' (Mult (Const 0.0) y) = zero
simplify' (Mult (Const 1.0) y) = simplify' y

simplify' (Mult (Const x) (Mult (Const y) z)) = Const (x*y) * simplify' z
simplify' (Mult (Mult (Const x) y) (Const z)) = Const (x*z) * simplify' y
simplify' (Mult x (Mult (Const y) (Const z))) = Const (y*z) * simplify' x
simplify' (Mult (Const x) (Plus y z)) = (Const x * simplify' y) + (Const x * simplify' z)

simplify' (Exp (Const x) (Const y)) = Const (pow x y)
simplify' (Exp x (Const 0.0)) = one
simplify' (Exp x (Const 1.0)) = x
simplify' (Exp (Exp x (Const y)) (Const z)) = x :**: (Const (y * z))
simplify' (Exp E (Log x)) = x
simplify' (Exp (Div x y) z) = x :**: z :/: y :**: z
simplify' (Exp (Mult x y) z) = x :**: z :*: y :**: z

simplify' (Div x (Const 1.0)) = simplify' x
simplify' (Div (Const 0.0) y) = zero
simplify' (Div (Const x) (Const y))
  | x == y = one
simplify' (Div (Div a b) (Div c d)) = (a :*: d) :/: (b :*: c)

simplify' (Log (Mult x y)) = Log x + Log y
simplify' (Log (Div y x)) = Log y - Log x
simplify' (Log (Exp x y)) = y * Log x
simplify' (Log E) = one

simplify' (Plus (Mult (Sin a) (Sin b)) (Mult (Cos c) (Cos d)))
  | a == b && b == c && c == d = one
simplify' (Sin (Plus a b)) = Sin a * Cos b + Cos a * Sin b
simplify' (Cos (Plus a b)) = Cos a * Cos b - Sin a * Sin b
simplify' (Sin (Const 0.0)) = zero
simplify' (Cos (Const 0.0)) = one
simplify' (Sin x) = Sin (simplify' x)
simplify' (Cos x) = Cos (simplify' x)

simplify' (Div x y) = simplify' x :/: simplify' y
simplify' (Exp x y) = simplify' x :**: simplify' y
simplify' (Mult x y) = simplify' x :*: simplify' y
simplify' (Plus x y) = simplify' x :+: simplify' y
simplify' x = x

fullSimplify :: Expr Number -> Expr Number
fullSimplify expr = fullSimplify' expr zero where
  fullSimplify' cur last | cur == last = cur
                         | otherwise = let cur' = simplify' cur
                                        in fullSimplify' cur' cur

mapVar :: forall a. (Char -> Expr a) -> Expr a -> Expr a
mapVar f (Var d) = f d
mapVar _ a@(Const _) = a
mapVar _ E = E
mapVar f (Plus x y) = mapVar f x :+: mapVar f y
mapVar f (Mult x y) = mapVar f x :*: mapVar f y
mapVar f (Exp x y) = mapVar f x :**: mapVar f y
mapVar f (Div x y) = mapVar f x :/: mapVar f y
mapVar f (Log x) = Log (mapVar f x)
mapVar f (Sin x) = Sin (mapVar f x)
mapVar f (Cos x) = Cos (mapVar f x)

plugIn :: forall a. Char -> a -> Expr a -> Expr a
plugIn c val = mapVar \ x -> if x == c then Const val else Var x

-- | Takes a variable to substitute, the value of the substitution, and an
-- | expression, and uniformly substitutes the variable with the value to
-- | give a numerical value.
evalExpr :: forall a. (Realizable a) => Char -> a -> Expr a -> Sentence Number
evalExpr c x = evalExpr' <<< plugIn c x

evalExpr' :: forall a. (Realizable a) => Expr a -> Sentence Number
evalExpr' (Const x) = pure (toReal x)
evalExpr' E = pure (exp 1.0)
evalExpr' (Var v) = throwError FreeVariable
evalExpr' (Plus x y) = add <$> evalExpr' x <*> evalExpr' y
evalExpr' (Mult x y) = mul <$> evalExpr' x <*> evalExpr' y
evalExpr' (Exp x y) = pow <$> evalExpr' x <*> evalExpr' y
evalExpr' (Div x y) = div <$> evalExpr' x <*> evalExpr' y
evalExpr' (Log x) = log <$> evalExpr' x
evalExpr' (Sin x) = sin <$> evalExpr' x
evalExpr' (Cos x) = cos <$> evalExpr' x

derivative :: forall a. (Realizable a) => Expr a -> Expr Number
derivative = derivative' <<< map toReal

derivative' :: Expr Number -> Expr Number
derivative' (Var _) = one
derivative' (Const _) = zero
derivative' E = zero
derivative' (Plus x y) = derivative' x + derivative' y
derivative' (Mult x y) = derivative' x * y + derivative' y * x
derivative' (Div x y) = (x * derivative' y - y * derivative' x) :/: (y * y)
derivative' (Log x) = derivative' x :/: x
derivative' (Exp x (Const c)) = Const c * x :**: Const (c - one) * derivative' x
derivative' (Exp E x) = derivative' x :*: E :**: x
derivative' (Exp c@(Const _) x) = Log c :*: c :**: x :**: derivative' x
derivative' (Sin x) = Cos x :*: derivative' x
derivative' (Cos x) = neg (Sin x) :*: derivative' x
derivative' x = derivative' (simplify' x)

-- | Takes the derivative, then simplifies it as much as possible.
ddx :: forall a. (Realizable a) => Expr a -> Expr Number
ddx = fullSimplify <<< derivative

nthDerivative :: forall a. (Realizable a) => Int -> Expr a -> Expr Number
nthDerivative n = nthDerivative' n <<< map toReal

nthDerivative' :: Int -> Expr Number -> Expr Number
nthDerivative' 0 expr = fullSimplify expr
nthDerivative' n expr = nthDerivative (n - 1) (derivative expr)

-- | `taylorExpansion n r v fn` computes the Taylor expansion of `fn` to `n`
-- | terms about `r`, where `fn` is an expression with a single free variable `v`.
taylorExpansion :: forall a. (Realizable a) => Int -> a -> Char -> Expr a -> Expr Number
taylorExpansion ord a v fx = fullSimplify (foldr (:+:) (nthTaylorTerm 0 a v fx) (taylorList ord a v fx))

foreign import modImpl :: Number -> Number -> Number

even :: Number -> Boolean
even n = n `modImpl` 2.0 == 0.0

fac :: Number -> Number
fac n | even n = facEven n
      | otherwise = facOdd n

facEven :: Number -> Number
facEven n = facEven' n 1.0 n n
  where
    facEven' _ prod _ 0.0 = prod
    facEven' orig prod sum curr = facEven' orig (prod * sum) (sum + (curr - 2.0)) (curr - 2.0)

facOdd :: Number -> Number
facOdd n = facOdd' n 1.0 n n
  where
    facOdd' _ _ _ 1.0 = 1.0
    facOdd' orig prod sum 3.0 = prod * sum * (orig / 2.0 + 0.5)
    facOdd' orig prod sum curr = facOdd' orig (prod * sum) (sum + (curr - 2.0)) (curr - 2.0)

nthTaylorTerm :: forall a. (Realizable a) => Int -> a -> Char -> Expr a -> Expr Number
nthTaylorTerm n a v fx =
  let den = Const (fac (toReal n))
      num = plugIn v (toReal a) (nthDerivative n fx)
      lhs = num :/: den
      rhs = (Var v - Const (toReal a)) :**: Const (toReal n)
   in fullSimplify (lhs :*: rhs)

taylorList :: forall a. (Realizable a) => Int -> a -> Char -> Expr a -> List (Expr Number)
taylorList ord a v fx = unfoldr taylorListHelper ord
  where
    taylorListHelper :: Int -> Maybe (Tuple (Expr Number) Int)
    taylorListHelper 0 = Nothing
    taylorListHelper n = Just (Tuple (nthTaylorTerm n a v fx) (n-1))
