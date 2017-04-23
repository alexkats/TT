module Evaluation where

import           Grammar ( Lambda
                         , LambdaDB (..)
                         , operateDB
                         )

mFree :: (Integer -> Integer) -> LambdaDB -> LambdaDB
mFree f = mFree' 0
  where
    mFree' d (DBVar n)   = if n < d
                           then DBVar n
                           else DBVar $ f n
    mFree' d (a :++ b)   = mFree' d a :++ mFree' d b
    mFree' d (LambdaD e) = LambdaD $ mFree' (d + 1) e

incFree :: LambdaDB -> LambdaDB
incFree = mFree (+1)

decFree :: LambdaDB -> LambdaDB
decFree = mFree $ \n -> n - 1

subDB :: Integer -> LambdaDB -> LambdaDB -> LambdaDB
subDB n (DBVar m) e   = if n == m
                        then e
                        else DBVar m
subDB n (a :++ b) e   = subDB n a e :++ subDB n b e
subDB n (LambdaD a) e = LambdaD $ subDB (n + 1) a $ incFree e

subRdx :: LambdaDB -> LambdaDB -> LambdaDB
subRdx a b = decFree $ subDB 0 a $ incFree b

toWHNF :: LambdaDB -> LambdaDB
toWHNF ((LambdaD a) :++ b) = toWHNF $ subRdx a b
toWHNF (a :++ b)           = case toWHNF a of
                               (LambdaD e) -> toWHNF $ subRdx e b
                               e           -> e :++ b
toWHNF l                   = l

norm :: Lambda -> Lambda
norm = operateDB $ \_ -> normDB

normDB :: LambdaDB -> LambdaDB
normDB l = case toWHNF l of
             (LambdaD e) -> LambdaD $ normDB e
             (a :++ b)   -> normDB a :++ normDB b
             v           -> v
