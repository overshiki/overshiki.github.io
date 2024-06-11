{-# LANGUAGE OverloadedStrings #-}
module Pretty where 
import Def 
import Data.Fix 

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P


ten, add, call :: Expr
ten  = Fix (Literal (IntLit 10))
add  = Fix (Literal (Ident "add" ))
call = Fix (Call add [ten, ten]) --add(10, 10)

type Algebra f a = f a -> a
prettyPrint :: Algebra ExprF Doc
prettyPrint (Literal (IntLit i)) = P.int i
prettyPrint (Literal (Ident s)) = P.text s
prettyPrint (Call f as)     = f <> P.parens (mconcat (P.punctuate "," as))  ---f(a,b...)
prettyPrint (Index it idx)  = it <> P.brackets idx                ---a[b]
prettyPrint (Unary op it)   = (P.text op) <> it                   ---op x
prettyPrint (Binary l op r) = l <> (P.text op) <> r               ---lhs op rhs
prettyPrint (Paren exp)     = P.parens exp                        ---(op)

v = foldFix prettyPrint call