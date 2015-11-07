module Test.Main where

import Prelude
import Math.Differentiable.Real
import Math.Differentiable.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Data.Either
import Math (pow, sqrt1_2, abs)

foreign import data PROCESS :: !
foreign import failure :: forall e. String -> Eff ( process :: PROCESS | e ) Unit

testNumber :: forall e. Sentence Number -> Number -> Eff ( process :: PROCESS, console :: CONSOLE | e ) Unit
testNumber sigma n = either (const $ failure "Expression must not have free variables")
                            (\ r -> if closeEnough n r then log "✓" else failure "Value is not equal to the evaluated expression")
                            sigma

testSentence :: forall e. Sentence Number -> Sentence Number -> Eff (process :: PROCESS, console :: CONSOLE | e) Unit
testSentence (Left _) _ = failure "First argument had free variables but must be a sentence"
testSentence _ (Left _) = failure "Second argument had free variables but must be a sentence"
testSentence (Right v) (Right v') = if closeEnough v v'
                                       then log "✓"
                                       else failure "Two sentences are not equal"

closeEnough :: Number -> Number -> Boolean
closeEnough x y = abs (x - y) < 0.000000005

taylorSine :: Expr Number
taylorSine = taylorExpansion 5 0 'x' (Sin (Var 'x'))

positiveCircleSlope :: Expr Number
positiveCircleSlope = ddx $ (one - Var 'x' :**: Const 2.0) :**: Const 0.5

eX :: Expr Number
eX = E :**: Var 'x'

sin :: Expr Number
sin = Sin (Var 'x')

main = do
  log "Taylor expansion of sin(x) about 0 truncated to O(5) should equal x - (x^3)/3! + (x^5)/5!"
  testNumber (evalExpr 'x' 2.0 taylorSine) (2.0 - (pow 2.0 3.0)/(3.0*2.0) + (pow 2.0 5.0)/(5.0*4.0*3.0*2.0))
  log "Slope of a circle x^2 + y^2 = 1 at x = +sqrt(1/2), y = +sqrt(1/2) is -1"
  testNumber (evalExpr 'x' sqrt1_2 positiveCircleSlope) (negate 1.0)
  log "Derivative of e^x is idempotent"
  testSentence (evalExpr 'x' 3.0 eX) (evalExpr 'x' 3.0 $ nthDerivative 5 eX)
  log "Derivatives of sin(x) evaluated at 0 cycle through [0, 1, 0, -1]"
  testNumber (evalExpr 'x' 0.0 $ nthDerivative 0 sin) 0.0
  testNumber (evalExpr 'x' 0.0 $ nthDerivative 1 sin) 1.0
  testNumber (evalExpr 'x' 0.0 $ nthDerivative 2 sin) 0.0
  testNumber (evalExpr 'x' 0.0 $ nthDerivative 3 sin) (negate 1.0)
