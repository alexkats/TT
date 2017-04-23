{-# LANGUAGE OverloadedStrings #-}

module TypeInference where

import qualified Data.ByteString.Char8    as C8
import           Utils                    (bracketedF)
import           Grammar                  ( VarName
                                          , Lambda (..)
                                          , free
                                          , notInContext
                                          )
import           Expression               ( ExprTerm (..)
                                          , ExprName
                                          , ExprSystem
                                          , Sub
                                          , unify
                                          , subAll
                                          )
import           Control.Monad.State.Lazy ( State
                                          , get
                                          , gets
                                          , modify
                                          , runState
                                          )
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Function            (on)
import           Data.List                (deleteBy)
import           Control.Applicative      ((<|>))

type Type = ExprTerm
type ExprPair = (ExprSystem, Type)
type TypePair = (Sub, Type)
type Ctx = [(VarName, ExprName)]

data Builder = BB { bvCtx :: Ctx
                  , fvCtx :: Ctx
                  , tVars :: [ExprName]
                  }

type ExprBuild = State Builder

infixr 7 -->
(-->) :: Type -> Type -> Type
a --> b = ExprFunc "->" [a, b]

findType :: VarName -> ExprBuild (Maybe VarName)
findType x = do
    (BB bvCtx fvCtx _) <- get
    return $ lookup x bvCtx <|> lookup x fvCtx

createVar :: (VarName -> Builder -> Builder) -> ExprBuild VarName
createVar f = do
    vs <- gets tVars
    let v = notInContext vs
    modify $ \bb -> bb { tVars = v : vs }
    modify (f v)
    return v

getVar :: ExprBuild VarName
getVar = createVar $ const id

createFreeVar :: VarName -> ExprBuild VarName
createFreeVar x = createVar $ \v bb -> bb { fvCtx = (x, v) : fvCtx bb }

createBoundedVar :: VarName -> ExprBuild VarName
createBoundedVar x = createVar $ \v bb -> bb { bvCtx = (x, v) : bvCtx bb }

deleteBoundedVar :: VarName -> ExprBuild ()
deleteBoundedVar x = modify $ \bb ->
                         let bv = deleteBy (on (==) fst) (x, "") $ bvCtx bb
                         in bb { bvCtx = bv }

getVarType :: VarName -> ExprBuild VarName
getVarType x = do
    t <- findType x
    case t of
        Nothing -> createFreeVar x
        Just t  -> return t

buildSystemM :: Lambda -> ExprBuild ExprPair
buildSystemM (Var x)        = do
    v <- getVarType x
    return ([], ExprVar v)
buildSystemM (p :+ q)       = do
    (p1, p2) <- buildSystemM p
    (q1, q2) <- buildSystemM q
    a <- getVar
    let b = [(p2, q2 --> ExprVar a)] ++ p1 ++ q1
    return (b, ExprVar a)
buildSystemM (Lambda x lam) = do
    a <- createBoundedVar x
    (expr, t) <- buildSystemM lam
    deleteBoundedVar x
    return (expr, ExprVar a --> t)

trim :: Ctx -> Ctx
trim = filter ((/= "") . fst)

build :: Lambda -> (ExprPair, Ctx)
build l = trimAll $ runState (buildSystemM l) (BB [] [] [])
  where
    trimAll (x, bb) = (x, trim $ fvCtx bb)

inferType :: Lambda -> Maybe TypePair
inferType l = do
    let ((expr, t), ctx) = build l
    s <- unify expr
    let mfvs = free l
        fvs = map getVT mfvs
        getVT x = let v = fromJust $ lookup x ctx
                  in let t = fromMaybe (ExprVar v) $ lookup v s
                     in (x, t)
    return (fvs, subAll s t)

showType :: Type -> String
showType (ExprVar v)         = C8.unpack v
showType (ExprFunc _ [x, y]) = bracketedF isFunc showType x ++ " -> " ++ showType y
  where
    isFunc (ExprFunc _ _) = True
    isFunc _              = False
