## Module Data.Differentiable.Class

#### `Realizable`

``` purescript
class Realizable a where
  toReal :: a -> Number
```

Types which can be represented as Numbers. This should be some kind of
embedding, for example the only admissible instance toReal :: Number -> Number
is the id function.

##### Instances
``` purescript
instance intRealizable :: Realizable Int
instance rationalRealizable :: Realizable Rational
instance numberRealizable :: Realizable Number
```

#### `Expr`

``` purescript
data Expr a
  = Const a
  | Var Char
  | E
  | Plus (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)
  | Exp (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Log (Expr a)
  | Sin (Expr a)
  | Cos (Expr a)
```

##### Instances
``` purescript
instance showExpr :: (Show a) => Show (Expr a)
instance eqExpr :: (Eq a) => Eq (Expr a)
instance semiGroupExpr :: Semigroup (Expr a)
instance monoidExpr :: (Monoid a) => Monoid (Expr a)
instance semiringExpr :: (Semiring a) => Semiring (Expr a)
instance ringExpr :: (Ring a) => Ring (Expr a)
instance functorExpr :: Functor Expr
```

#### `EvalError`

``` purescript
data EvalError
  = FreeVariable
```

##### Instances
``` purescript
instance showEvalError :: Show EvalError
```

#### `Sentence`

``` purescript
type Sentence = Either EvalError
```

#### `(:+:)`

``` purescript
(:+:) :: forall a. Expr a -> Expr a -> Expr a
```

_left-associative / precedence 6_

#### `(:*:)`

``` purescript
(:*:) :: forall a. Expr a -> Expr a -> Expr a
```

_left-associative / precedence 7_

#### `(:**:)`

``` purescript
(:**:) :: forall a. Expr a -> Expr a -> Expr a
```

_left-associative / precedence 8_

#### `(:/:)`

``` purescript
(:/:) :: forall a. Expr a -> Expr a -> Expr a
```

_left-associative / precedence 7_

#### `neg`

``` purescript
neg :: forall a. (Ring a) => Expr a -> Expr a
```

#### `(:-:)`

``` purescript
(:-:) :: forall a. (Ring a) => Expr a -> Expr a -> Expr a
```

_left-associative / precedence 6_


