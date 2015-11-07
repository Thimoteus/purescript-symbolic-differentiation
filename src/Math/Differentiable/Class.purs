module Math.Differentiable.Class
  ( Expr (..)
  , Realizable
  , Sentence()
  , EvalError(..)
  , toReal
  , neg
  , (:+:)
  , (:-:)
  , (:*:)
  , (:**:)
  , (:/:)
  ) where

import Prelude
import Data.Char (toString)
import Data.Either (Either(..), either)
import Data.Monoid (Monoid, mempty)
import qualified Data.Int as Int
import qualified Data.Rational as Rational

-- | Types which can be represented as Numbers. This should be some kind of
-- | embedding, for example the only admissible instance toReal :: Number -> Number
-- | is the id function.
class Realizable a where
  toReal :: a -> Number

instance intRealizable :: Realizable Int where
  toReal = Int.toNumber

instance rationalRealizable :: Realizable Rational.Rational where
  toReal = Rational.toNumber

instance numberRealizable :: Realizable Number where
  toReal = id

data Expr a = Const a
            | Var Char
            | E
            | Plus (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Exp (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Log (Expr a)
            | Sin (Expr a)
            | Cos (Expr a)

data EvalError = FreeVariable
type Sentence = Either EvalError
instance showEvalError :: Show EvalError where
  show FreeVariable = "Cannot evaluate an expression with free variables"

infixl 6 :+:
(:+:) :: forall a. Expr a -> Expr a -> Expr a
(:+:) = Plus

infixl 7 :*:
(:*:) :: forall a. Expr a -> Expr a -> Expr a
(:*:) = Mult

infixl 8 :**:
(:**:) :: forall a. Expr a -> Expr a -> Expr a
(:**:) = Exp

infixl 7 :/:
(:/:) :: forall a. Expr a -> Expr a -> Expr a
(:/:) = Div

instance showExpr :: (Show a) => Show (Expr a) where
  show (Const a) = show a
  show (Var c) = toString c
  show E = "e"
  show (Plus x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Mult x y) = "(" ++ show x ++ " * "  ++ show y ++ ")"
  show (Exp x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Log a) = "ln (" ++ show a ++ ")"
  show (Sin a) = "sin (" ++ show a ++ ")"
  show (Cos a) = "cos (" ++ show a ++ ")"

instance eqExpr :: (Eq a) => Eq (Expr a) where
  eq (Const x) (Const y) = x == y
  eq (Var x) (Var y) = x == y
  eq E E = true
  eq (Plus x m) (Plus y n) = x == y && m == n
  eq (Mult x m) (Mult y n) = x == y && m == n
  eq (Exp x m) (Exp y n) = x == y && m == n
  eq (Div x m) (Div y n) = x == y && m == n
  eq (Log x) (Log y) = x == y
  eq (Sin x) (Sin y) = x == y
  eq (Cos x) (Cos y) = x == y
  eq _ _ = false

instance semiGroupExpr :: Semigroup (Expr a) where
  append = Plus

instance monoidExpr :: (Monoid a) => Monoid (Expr a) where
  mempty = Const mempty

instance semiringExpr :: (Semiring a) => Semiring (Expr a) where
  one = Const one
  zero = Const zero
  mul = Mult
  add = Plus

neg :: forall a. (Ring a) => Expr a -> Expr a
neg v@(Var _) = Const (negate one) * v
neg (Const c) = Const (negate c)
neg E = Const (negate one) :*: E
neg (Plus x y) = neg x :+: neg y
neg (Mult x y) = neg x :*: y
neg e@(Exp _ _) = Const (negate one) :*: e
neg (Div x y) = neg x :/: y
neg l@(Log _) = Const (negate one) :*: l
neg s@(Sin _) = Const (negate one) :*: s
neg c@(Cos _) = Const (negate one) :*: c

infixl 6 :-:
(:-:) :: forall a. (Ring a) => Expr a -> Expr a -> Expr a
(:-:) x y = x + neg y

instance ringExpr :: (Ring a) => Ring (Expr a) where
  sub = (:-:)

instance functorExpr :: Functor Expr where
  map f (Const x) = Const (f x)
  map f (Var c) = Var c
  map f E = E
  map f (Plus x y) = map f x :+: map f y
  map f (Mult x y) = map f x :*: map f y
  map f (Div x y) = map f x :/: map f y
  map f (Exp x y) = map f x :**: map f y
  map f (Log x) = Log (map f x)
  map f (Sin x) = Sin (map f x)
  map f (Cos x) = Cos (map f x)
