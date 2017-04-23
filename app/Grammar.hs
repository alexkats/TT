module Grammar where

import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Utils                 ( bracketed
                                       , revlookup
                                       )

type VarName = C.ByteString

data Lambda = Var VarName
            | Lambda VarName Lambda
            | Lambda :+ Lambda
              deriving (Ord, Eq)

instance Show Lambda where
    show (Var v)      = C.unpack v
    show (Lambda v e) = "\\" ++ C.unpack v ++ "." ++ show e
    show (a :+ b)     = bracketed isLambda a ++ " " ++ bracketed isNotVar b
      where
        isLambda (Lambda _ _) = True
        isLambda _            = False
        isNotVar (Var _)      = False
        isNotVar _            = True

data LambdaDB = DBVar Integer
              | LambdaD LambdaDB
              | LambdaDB :++ LambdaDB
                deriving (Ord, Eq)

instance Show LambdaDB where
    show (DBVar n)   = show n
    show (LambdaD e) = "(\\" ++ show e ++ ")"
    show (a :++ b)   = "(" ++ show a ++ " " ++ show b ++ ")"

free :: Lambda -> [VarName]
free (Var v)      = [v]
free (Lambda v e) = filter (not . (== v)) $ free e
free (a :+ b)     = free a ++ free b

type DBContext = [(VarName, Integer)]

constructContext :: Lambda -> DBContext
constructContext = enumerate . free
  where
    enumerate l = zip l [1..]

getNum :: VarName -> DBContext -> Integer
getNum v = fromJust . lookup v

getVar :: Integer -> DBContext -> VarName
getVar n = fromJust . revlookup n

inc :: DBContext -> DBContext
inc = map $ \(v, n) -> (v, n + 1)

notInContext :: [VarName] -> VarName
notInContext context = head $ filter (not . inContext) $ varAndTicks
  where
    inContext var = var `elem` context
    varAndTicks = map C.pack $ concat $ iterate (map (++ "\'")) vars
    vars = map (:[]) ['a'..'z']

toDB :: DBContext -> Lambda -> LambdaDB
toDB context (Var v)      = DBVar $ getNum v context
toDB context (a :+ b)     = toDB context a :++ toDB context b
toDB context (Lambda v e) = LambdaD $ toDB newContext e
  where
    newContext = (v, 0) : inc context

fromDB :: DBContext -> LambdaDB -> Lambda
fromDB context (DBVar n)   = Var $ getVar n context
fromDB context (a :++ b)   = fromDB context a :+ fromDB context b
fromDB context (LambdaD e) = Lambda v $ fromDB newContext e
  where
    v = notInContext $ map fst context
    newContext = (v, 0) : inc context

operateDB :: (DBContext -> LambdaDB -> LambdaDB) -> Lambda -> Lambda
operateDB f lambda = fromDB context $ f context lambdaDB
  where
    context = constructContext lambda
    lambdaDB = toDB context lambda
