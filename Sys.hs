module Sys where

import Absmyjs

import Data.List
import qualified Data.Map as M
import qualified Memory as Mem
import qualified Env as Env
import Types

append v2 v1 = StringVal $ (show v1) ++ (show v2)

sfLog params = do
      let toPrint = StringVal $ (intercalate " " (map show params)) ++ "\n" in do
          Env.modify (append toPrint) "output"
          return UndefinedVal

qIdentResolve (QIdent identList) = _qIdentResolve identList
              where _qIdentResolve [] = []
                    _qIdentResolve (id:ids) =
                         case id of
                              IdentPart (IIdentBare (Ident ident)) ->
                                        (LiteralExpr (StringLiteral ident)):(_qIdentResolve ids)
                              IdentPart (IIdentIndexed (Ident ident) exp) ->
                                        (LiteralExpr (StringLiteral ident)):exp:(_qIdentResolve ids)

paramsToList (ParamNameList ids) = map (\(Ident i) -> i) ids

isSysFunc name = name == "log"

callSysFunc (BuiltinFunctionVal name) params = case name of
         "log" -> sfLog params
         
sysFunc name = BuiltinFunctionVal $ case name of
        "log" -> "log"