module ExprT (
    ExprT(..),
    Expr(lit, add, mul),
    VarExprT(..),
    HasVar(..)
) where

-- | Data type of arithmetic expressions
-- Is capable of representing expressions invoving integer constants, addition
-- and multiplication
-- Example: the expression (2+3)*4 would be represented as 
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
    deriving (Show, Eq)

-- | For abstracting the properties of the data type ExprT
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- | Make an instance of Expr for the ExprT
instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- | Types that are instances of HasVar have some notion of named variables
class HasVar a where
    var :: String -> a

  
data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance HasVar VarExprT where
    var = Var 

instance Expr VarExprT where
    lit = VarLit
    add = VarAdd
    mul = VarMul