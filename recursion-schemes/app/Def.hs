{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Def where
import Data.Fix 
import Text.Show.Deriving
import Control.Arrow

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data ExprF a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor)

$(deriveShow1 ''ExprF)

type Expr = Fix ExprF

-- -- the Fix datatype is just 
-- newtype Fix f = Fix { unFix :: f (Fix f) }

e1 = Literal (IntLit 10) :: ExprF a

d1 = Fix (Literal (IntLit 10)) :: Fix ExprF
d2 = Fix $ Paren $ Fix (Literal (IntLit 10)) :: Fix ExprF

l1 = Literal (IntLit 10) :: ExprF a
l2 = Literal (IntLit 10) :: ExprF ()
l3 = Literal (IntLit 10) :: ExprF (ExprF a)
l4 = Literal (IntLit 10) :: ExprF (Fix ExprF)


bottomUp :: Functor a => (Fix a -> Fix a) -> Fix a -> Fix a
bottomUp fn =
  unFix                    -- 1) unpack
  >>> fmap (bottomUp fn) -- 2) recurse
  >>> Fix                 -- 3) repack
  >>> fn                 -- 4) apply

flattenTerm :: Expr -> Expr
flattenTerm (Fix (Paren e)) = e  -- remove all Parens
flattenTerm other = other       -- do nothing otherwise

flatten'' :: Expr -> Expr
flatten'' = bottomUp flattenTerm




