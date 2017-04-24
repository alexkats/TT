{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Prelude                          hiding (takeWhile)
import           Grammar                          (Lambda (..), VarName)
import           Expression                       ( Expression
                                                  , ExprSystem
                                                  , ExprTerm (..)
                                                  )
import           Data.Attoparsec.ByteString.Char8 as PC8
import           Control.Applicative              ((<|>))
import qualified Data.ByteString.Char8            as C8

spaces1 = PC8.takeWhile (== ' ')

lexeme :: PC8.Parser a -> PC8.Parser a
lexeme p = p <* PC8.skipWhile (== ' ')

parens :: PC8.Parser a -> PC8.Parser a
parens p = lexeme (PC8.char '(') *> lexeme p <* PC8.char ')'

comma :: PC8.Parser Char
comma = lexeme $ PC8.char ','

almostVarName :: PC8.Parser C8.ByteString
almostVarName = PC8.takeWhile $ check
  where
    check c = PC8.isAlpha_ascii c || PC8.isDigit c || c == '\''

prependedVarName :: PC8.Parser Char -> PC8.Parser C8.ByteString
prependedVarName p = do
    c <- p
    r <- almostVarName
    return $ C8.cons c r

varName :: PC8.Parser C8.ByteString
varName = prependedVarName PC8.letter_ascii

expr :: PC8.Parser Lambda
expr = do
    l <- PC8.sepBy1 atom spaces1
    return $ foldl1 (:+) l

lambda :: PC8.Parser Lambda
lambda = do
    lexeme $ PC8.char '\\'
    v <- lexeme varName
    lexeme $ PC8.char '.'
    e <- expr
    return $ Lambda v e

atom :: PC8.Parser Lambda
atom = var <|> lambda <|> parens expr

var :: PC8.Parser Lambda
var = Var <$> varName

apply :: PC8.Parser Lambda -> PC8.Parser Lambda -> PC8.Parser Lambda
apply p q = do
    ex1 <- p <* spaces1
    ex2 <- q
    return $ ex1 :+ ex2

parseExpression :: C8.ByteString -> Either String Lambda
parseExpression = PC8.parseOnly expr

subExpression :: PC8.Parser (VarName, Lambda)
subExpression = do
    lexeme $ PC8.string "["
    v <- lexeme varName
    lexeme $ PC8.string ":="
    e <- lexeme expr
    PC8.string "]"
    return (v, e)

exprSystem :: PC8.Parser ExprSystem
exprSystem = PC8.sepBy1 expression PC8.endOfLine

expression :: PC8.Parser Expression
expression = do
    t1 <- lexeme term
    lexeme $ PC8.string "="
    t2 <- term
    return (t1, t2)

term :: PC8.Parser ExprTerm
term = function <|> exprVar

function :: PC8.Parser ExprTerm
function = do
    fName <- funcName
    args <- parens $ PC8.sepBy1 term comma
    return $ ExprFunc fName args

funcName :: PC8.Parser C8.ByteString
funcName = prependedVarName $ PC8.satisfy $ PC8.inClass ['a'..'h']

exprVar :: PC8.Parser ExprTerm
exprVar = exprVarName >>= return . ExprVar

exprVarName :: PC8.Parser C8.ByteString
exprVarName = prependedVarName $ PC8.satisfy $ PC8.inClass ['i'..'z']

parseExpressions :: C8.ByteString -> Either String ExprSystem
parseExpressions = PC8.parseOnly exprSystem
