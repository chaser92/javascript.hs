{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Memory where

import Types
import qualified Data.Map as M
import Control.Monad.Trans.Except
import Control.Monad.State


type Store = M.Map Loc Val
data Memory = Mem { store :: Store, lastLoc :: Loc }

empty = Mem M.empty $ -1

init :: MonadTrans t0 => Monad m0 => MonadState Memory (t0 (ExceptT [Char] m0)) => t0 (ExceptT [Char] m0) ()
init = do
     alloc
     setReturnVal UndefinedVal

getVar loc = do
       (Mem store _) <- get
       case M.lookup loc store of
            Nothing -> lift $ throwE $ "Invalid memory location read: " ++ (show loc)
            Just k -> return k

updateVar fun loc = do
          (Mem store lastLoc) <- get
          case M.lookup loc store of
               Nothing -> lift $ throwE $ "Invalid memory location written: " ++ (show loc)
               Just k -> put $ Mem (M.insert loc (fun k) store) lastLoc

alloc :: MonadState Memory m => m Loc
alloc = do
      (Mem store lastLoc) <- get
      put $ Mem (M.insert (lastLoc + 1) UndefinedVal store) (lastLoc + 1)
      return $ lastLoc + 1

dealloc loc = modify (\(Mem store lastLoc) ->
          Mem (M.delete loc store) lastLoc)

setVar loc value = updateVar (\x -> value) loc

setReturnVal :: MonadTrans t0 => Monad m0 => MonadState Memory (t0 (ExceptT [Char] m0)) => Val -> t0 (ExceptT [Char] m0) ()
setReturnVal val = setVar 0 val

clearReturnVal :: MonadTrans t0 => Monad m0 => MonadState Memory (t0 (ExceptT [Char] m0)) => t0 (ExceptT [Char] m0) ()
clearReturnVal = setReturnVal UndefinedVal

getReturnVal :: MonadTrans t0 => Monad m0 => MonadState Memory (t0 (ExceptT [Char] m0)) => t0 (ExceptT [Char] m0) Val
getReturnVal = getVar 0