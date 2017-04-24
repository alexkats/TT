{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO                        ( Handle
                                                  , hPutStr
                                                  , hPutStrLn
                                                  , stdin
                                                  )
import           System.Environment               (getArgs)
import           Data.Attoparsec.ByteString.Char8 as PC8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8
import qualified Evaluation                       as Eval
import           Expression                       ( unify
                                                  , showExpressions
                                                  )
import           Grammar                          (Lambda (..), VarName, free)
import           TypeInference                    (inferType, showType)
import qualified Parser                           as Par
import qualified Subs                             as Subs
import           Utils                            ( fileIO
                                                  , getEitherValue
                                                  , monadicPair
                                                  )

main :: IO ()
main = getArgs >>= \args ->
    case args of
        []    -> error "No arguments"
        ["1"] -> hw1
        ["2"] -> hw2
        ["3"] -> hw3
        ["4"] -> hw4
        ["5"] -> hw5
        ["6"] -> hw6
        _     -> error "Error occurred"

hw1 :: IO ()
hw1 = fileIO "tests/task1.in" "tests/task1.out" $ \input output ->
    hReadEx input Par.expr >>= C8.hPutStrLn output . showAll

hw2 :: IO ()
hw2 = fileIO "tests/task2.in" "tests/task2.out" $ \input output ->
    hReadEx input Par.expr >>= C8.hPutStr output . C8.unlines . free

hw3 :: IO ()
hw3 = fileIO "tests/task3.in" "tests/task3.out" $ \input output ->
    hReadEx input (monadicPair (Par.lexeme Par.expr, Par.subExpression)) >>= hPutStrLn output . subThis

hw4 :: IO ()
hw4 = fileIO "tests/task4.in" "tests/task4.out" $ \input output -> do
        term <- hReadEx input Par.expr
        hPutStrLn output $ show $ Eval.norm term

hw5 :: IO ()
hw5 = fileIO "tests/task5.in" "tests/task5.out" $ \input output -> do
        input' <- C8.hGetContents input
        let system = getEitherValue $ Par.parseExpressions input'
            solution = unify system
            ans = maybe "Система несовместна" showExpressions solution
        hPutStrLn output ans

hw6 :: IO ()
hw6 = fileIO "tests/task6.in" "tests/task6.out" $ \input output -> do
        lam <- hReadEx input Par.expr
        case inferType lam of
            Nothing     -> hPutStrLn output "Лямбда-выражение не имеет типа."
            Just (s, t) -> do
                hPutStrLn output $ showType t
                hPutStr output $ unlines $ map showExpr s
                  where
                    showExpr (x, t') = C8.unpack x ++ " : " ++ showType t'

showAll :: Lambda -> C8.ByteString
showAll (Var v)      = v
showAll (Lambda v e) = "(\\" `mappend` v `mappend` "." `mappend` showAll e `mappend` ")"
showAll (e :+ e')    = "(" `mappend` showAll e `mappend` " " `mappend` showAll e' `mappend` ")"

subThis :: (Lambda, (VarName, Lambda)) -> String
subThis (to, (v, x)) = case Subs.subAll v x to of
                           Just e  -> show e
                           Nothing -> "Нет свободы для подстановки для переменной " `mappend` C8.unpack (head $ Subs.noFreeFVars x v to)

readEx :: PC8.Parser a -> IO a
readEx = hReadEx stdin

hReadEx :: Handle -> PC8.Parser a -> IO a
hReadEx h p = BS.hGetLine h >>= return . getEitherValue . PC8.parseOnly p
