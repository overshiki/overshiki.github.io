{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Def2 where 
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

import Control.Monad.State.Lazy
import Control.Monad 
import Data.Fix 
data Lit
  = StrLit String
  | IntLit Int
  | Ident String

data ExprF a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

type Env = State [Doc]
prettyPrint :: ExprF Doc -> Env String
prettyPrint (Literal (IntLit i)) = return $ show $ P.int i
prettyPrint (Literal (Ident s)) = return $ show $ P.text s
prettyPrint (Call f as) = do 
  -- f(a,b...)
  -- return $ show $ f <> P.parens (mconcat (P.punctuate "," as))
  return $ f ++ (foldl1 ( ++ ) as)

-- prettyPrint (Index it idx) = do
--   -- a[b]
--   return $ show $ it <> P.brackets idx    

-- prettyPrint (Unary op it) = do 
--   -- op x
--   return $ show $ (P.text op) <> it

-- prettyPrint (Binary l op r) = do
--   -- lhs op rhs
--   return $ show $ l <> (P.text op) <> r          

-- prettyPrint (Paren exp) = do 
--   -- (op)
--   return $ show $ P.parens exp    

foldPrint :: Expr -> Env String
foldPrint = foldFixM prettyPrint