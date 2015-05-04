module Env where

import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import qualified Data.Map as M
import qualified Memory as Mem


empty = M.empty

resolve name = do
        env <- ask
        case M.lookup name env of
             Nothing -> lift (throwE $ name ++ " is not defined")
             Just k -> return k

assign name = do
       env <- ask
       loc <- Mem.alloc
       return $ M.insert name loc env

modify f name = do
       loc <- resolve name
       Mem.updateVar f loc
       return()

set name value = do
       loc <- resolve name
       Mem.setVar loc value
       return()

get name = do
       loc <- resolve name
       Mem.getVar loc
