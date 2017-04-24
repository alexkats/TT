module Subs where

import           Grammar   (Lambda (..), VarName, free)
import           Data.List (sort, nub, intersect)

boundedHere :: VarName -> Lambda -> [VarName]
boundedHere v = sort . nub . f []
  where
    f b (Var v')      = if v == v' then b else []
    f b (Lambda v' e) = f (v':b) e
    f b (x :+ y)      = f b x ++ f b y

noFreeFVars :: Lambda -> VarName -> Lambda -> [VarName]
noFreeFVars x v to = intersect (free x) $ boundedHere v to

freeForSub :: Lambda -> VarName -> Lambda -> Bool
freeForSub x v to = noFreeFVars x v to == []

sub :: VarName -> Lambda -> Lambda -> Lambda
sub v e (Var v')
    | v == v'   = e
    | otherwise = Var v'
sub v e (Lambda v' e')
    | v /= v'   = Lambda v' $ sub v e e'
    | otherwise = Lambda v' e'
sub v e (a :+ b) = sub v e a :+ sub v e b

subAll :: VarName -> Lambda -> Lambda -> Maybe Lambda
subAll v e e'
    | freeForSub e v e' = Just $ sub v e e'
    | otherwise         = Nothing
