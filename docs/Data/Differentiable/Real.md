## Module Data.Differentiable.Real

#### `evalExpr`

``` purescript
evalExpr :: forall a. (Realizable a) => Char -> a -> Expr a -> Sentence Number
```

Takes a variable to substitute, the value of the substitution, and an
expression, and uniformly substitutes the variable with the value to
give a numerical value.

#### `derivative`

``` purescript
derivative :: forall a. (Realizable a) => Expr a -> Expr Number
```

#### `ddx`

``` purescript
ddx :: forall a. (Realizable a) => Expr a -> Expr Number
```

Takes the derivative, then simplifies it as much as possible.

#### `nthDerivative`

``` purescript
nthDerivative :: forall a. (Realizable a) => Int -> Expr a -> Expr Number
```

#### `taylorExpansion`

``` purescript
taylorExpansion :: forall a. (Realizable a) => Int -> a -> Char -> Expr a -> Expr Number
```

`taylorExpansion n r v fn` computes the Taylor expansion of `fn` to `n`
terms about `r`, where `fn` is an expression with a single free variable `v`.


