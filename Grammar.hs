{-# LANGUAGE UnicodeSyntax #-}

module Grammar where

import Prelude.Unicode
import qualified Data.ByteString.Char8 as BS
import Data.List.Unicode
import Data.Maybe
import Utils

type VarName = BS.ByteString

data Lambda = Var VarName | Lambda VarName Lambda | Lambda 
