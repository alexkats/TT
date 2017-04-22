{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}

module Utils where

import System.IO

fileIO :: String -> String -> (Handle -> Handle -> IO a) -> IO a
fileIO input output action = do
    hInput <- openFile input ReadMode
    hOutput <- openFile output WriteMode
    result <- action hInput hOutput
    hClose hInput
    hClose hOutput
    return result

revlookup :: Eq b => b -> [(a, b)] -> Maybe a
revlookup _ []     = Nothing
revlookup b (p:ps) = if b == snd p
                     then Just $ fst p
                     else revlookup b ps

pairlookup :: Eq a => a -> [(a, b)] -> Maybe (a, b)
pairlookup _ []    = Nothing
pairlookup a (p:ps) = if a == fst p
                      then Just p
                      else pairlookup a ps

monadicPair :: Monad m => (m a, m b) -> m (a, b)
monadicPair (f, g) = do
    a <- f
    b <- g
    return (a, b)

getEitherValue :: Either a b -> b
getEitherValue (Left _)  = error "Left value"
getEitherValue (Right a) = a

bracketedF :: (a -> Bool) -> (a -> String) -> a -> String
bracketedF p toString a
    | p a       = "(" ++ toString a ++ ")"
    | otherwise = toString a

bracketed :: Show a => (a -> Bool) -> a -> String
bracketed p = bracketedF p show
