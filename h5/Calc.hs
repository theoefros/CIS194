{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc (
    eval,
    evalStr
) where

import ExprT (ExprT(..), Expr(..), HasVar(..), VarExprT(..))
import Parser (parseExp)
import qualified StackVM as SVM
import qualified Data.Map as M
import Control.Applicative (pure, (<*>), liftA2)

-- Wrapping Integers in newtype wrappers
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- Instances of Expr

-- Integer - works like a normal calculator
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)
    
-- Bool - every literal value less than or equal to 0 is interpreted as False
-- and all positive Integers are interpreted as True
-- Addition is logical 'or' and multiplication is logical 'and'
instance Expr Bool where
    lit n = if n <= 0 then False else True
    add = (||)
    mul = (&&)
    
-- Addition is max function and multiplication is the min function
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)
    
-- All values should be in the range 0..6 and arithmetic is done modulo 7
instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

-- Make Program an instance of Expr
instance Expr SVM.Program where
    lit n = [SVM.PushI n]
    add a b = a ++ b ++ [SVM.Add] 
    mul a b = a ++ b ++ [SVM.Mul]
    
-- | An evaluator for ExprT
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- | Evaluates arithmetic expressions given as String, producing Nothing for
-- inputs which are not well formed, and Just n for well-formed inputs that 
-- evaluate to n
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                  Just expr -> Just (eval expr)
                  Nothing   -> Nothing
                  
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * 4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul


-- Usig Data.Map to store mappings from variables to values
vars :: M.Map String Integer
vars = M.fromList [("x", 2),("y", 3)]
-- 
instance HasVar (M.Map String Integer -> Maybe Integer) where
  var = M.lookup
  
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = const (Just n)
    -- add f g = \m -> pure (+) <*> f m <*> g m 
    -- add f g = \m -> (+) <$> f m <*> g m 
    add f g = \m -> liftA2 (+) (f m) (g m)
    -- mul f g = \m -> pure (*) <*> f m <*> g m
    -- mul f g = \m -> (*) <$> f m <*> g m
    mul f g = \m -> liftA2 (*) (f m) (g m)
    
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs