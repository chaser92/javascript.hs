{-# LANGUAGE FlexibleContexts #-}

module Memory where

import Types
import qualified Data.Map as M
import Control.Monad.Trans.Except
import Control.Monad.State

type Loc = Int

type Store = M.Map Loc Val
data Memory = Mem { store :: Store, lastLoc :: Loc }

empty = Mem M.empty 0

getVar loc = do
       (Mem store _) <- get
       case M.lookup loc store of
            Nothing -> lift $ throwE "Invalid memory location accessed"
            Just k -> return k

updateVar fun loc = modify (\(Mem store lastLoc) ->
          Mem (M.insert loc (fun $ M.lookup loc store) store) lastLoc)

alloc :: MonadState Memory m => m Loc
alloc = do
      (Mem store lastLoc) <- get
      put $ Mem (M.insert (lastLoc + 1) UndefinedVal store) (lastLoc + 1)
      return $ lastLoc + 1

dealloc loc = modify (\(Mem store lastLoc) ->
          Mem (M.delete loc store) lastLoc)

setVar name value = updateVar (\x -> value) name

