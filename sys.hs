module Sys where

import Absmyjs

import qualified Data.Map as M
import qualified Memory as Mem
import qualified Env as Env
import Types

append v2 v1 = StringVal $ (show v1) ++ (show v2)
print text = Env.modify (append text) "output"

qIdentResolve (QIdent identList) = _qIdentResolve identList
              where _qIdentResolve [] = []
                    _qIdentResolve (id:ids) =
                         case id of
                              IdentPart (IIdentBare (Ident ident)) ->
                                        (LiteralExpr (StringLiteral ident)):(_qIdentResolve ids)
                              IdentPart (IIdentIndexed (Ident ident) exp) ->
                                        (LiteralExpr (StringLiteral ident)):exp:(_qIdentResolve ids)