{-# LANGUAGE OverloadedStrings #-}

module Expression where

import qualified Data.ByteString.Char8 as C8
import           Data.List             (intercalate)

type ExprName = C8.ByteString

data ExprTerm = ExprVar ExprName
              | ExprFunc ExprName [ExprTerm]
                deriving (Ord, Eq)

instance Show ExprTerm where
    show (ExprVar v)     = C8.unpack v
    show (ExprFunc f ts) = C8.unpack f ++ "(" ++ intercalate ", " (map show ts) ++ ")"

type Expression = (ExprTerm, ExprTerm)
type ExprSystem = [Expression]
type Sub = [(ExprName, ExprTerm)]

unify :: ExprSystem -> Maybe Sub
unify []             = Just []
unify ((a, b):exprs) = do
    s <- unify exprs
    t <- unifyExpr (subAll s a, subAll s b)
    return $ toEnd $ t ++ s

unifyExpr :: Expression -> Maybe Sub
unifyExpr (ExprVar a, ExprVar b)                   = if a == b
                                                     then Just []
                                                     else Just [(a, ExprVar b)]
unifyExpr (ExprVar a, f@(ExprFunc _ _))            = check a f
unifyExpr (f@(ExprFunc _ _), ExprVar a)            = check a f
unifyExpr (f@(ExprFunc f' ps), g@(ExprFunc g' qs)) = if f == g
                                                     then Just []
                                                     else if f' /= g' || length ps /= length qs
                                                          then Nothing
                                                          else unify $ zip ps qs

toEnd :: Sub -> Sub
toEnd []           = []
toEnd (expr:exprs) = expr : toEnd (subAll' expr exprs)
  where
    subAll' p l = map (subPairs p) l
    subPairs (x, f) (y, g) = (y, sub x f g)

check :: ExprName -> ExprTerm -> Maybe Sub
check a f = if isIn a f then Nothing else Just [(a, f)]

isIn :: ExprName -> ExprTerm -> Bool
isIn v (ExprVar t)     = v == t
isIn v (ExprFunc _ ts) = any (isIn v) ts

subAll :: Sub -> ExprTerm -> ExprTerm
subAll s t = foldr (uncurry sub) t s

sub :: ExprName -> ExprTerm -> ExprTerm -> ExprTerm
sub v t (ExprVar v')    = if v == v'
                          then t
                          else ExprVar v'
sub v t (ExprFunc f ts) = ExprFunc f $ map (sub v t) ts

showExpressions :: Sub -> String
showExpressions exprs = unlines $ map showExpr exprs
  where
    showExpr (a, b) = C8.unpack a ++ " = " ++ show b
