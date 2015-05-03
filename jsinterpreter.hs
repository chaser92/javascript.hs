module JSInterpreter where

import Absmyjs
import qualified Memory as Mem
import qualified Env as Env
import qualified Sys as Sys
import Types
import qualified Data.Map as M

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.State

exec (Progr stmts) = case runState (runExceptT (runReaderT (_exec stmts) Env.empty)) Mem.empty of
     (Left err, _) -> putStrLn $ "Runtime error: " ++ err
     (Right output, _) -> putStrLn $ show output
     where _exec stmts = do
           env <- iDecl (VarDeclAssign (Ident "output") (LiteralExpr (StringLiteral "")))
           local (\x -> env) (iStmt (CSS (CS stmts)))
           local (\x -> env) (Env.get "output")

iAssignQ [] val = lift $ throwE "Empty qualified list is not to be resolved"
iAssignQ [expr] val = do
         firstName <- iExpr expr
         Env.set (show firstName) val
iAssignQ (expr:exprs) val = do
         firstName <- iExpr expr
         oldVal <- Env.get (show firstName)
         newVal <- _iAssignQ oldVal exprs val
         Env.set (show firstName) newVal where
                   _iAssignQ obj [expr] val = do
                         prop <- iExpr expr
                         case obj of
                              ObjectVal m -> return $ ObjectVal $ M.insert (show prop) val m
                              o -> lift $ throwE $ (show o) ++ " is not an object"
                   _iAssignQ obj (expr:exprs) val = do
                         prop <- iExpr expr
                         case obj of
                              ObjectVal m -> do
                                       let newObj = M.lookup (show prop) m in
                                         case newObj of
                                            Just _newObj -> do
                                                 newVal <- _iAssignQ _newObj exprs val
                                                 return $ ObjectVal $ M.insert (show prop) newVal m
                                            Nothing -> lift $ throwE $ "Object " ++ (show obj) ++ " has no property " ++ (show prop)
                              o -> lift $ throwE $ (show o) ++ " is not an object"

iRetrQ [] = lift $ throwE "Empty qualified list is not to be resolved"
iRetrQ [expr] = do
       firstName <- iExpr expr
       Env.get (show firstName)
iRetrQ (expr:exprs) = do
       rootN <- iExpr expr
       root <- Env.get (show rootN)
       _iRetrQ root exprs where
                _iRetrQ root [] = return root
                _iRetrQ root (expr:exprs) = do
                        case root of
                             ObjectVal map -> do
                                  nextN <- iExpr expr
                                  case M.lookup (show nextN) map of
                                       Just x -> _iRetrQ x exprs
                                       Nothing -> _iRetrQ UndefinedVal exprs
                             o -> lift $ throwE $ (show o) ++ " is not an object" 

iDecl (VarDecl (Ident name)) = do
      env <- ask
      Env.assign name

iDecl (VarDeclAssign (Ident name) expr) = do
      env <- ask 
      r <- Env.assign name
      val <- iExpr expr
      local (\_ -> r) (Env.set name val)
      return r

iStmt (CSS (CS statements)) = _exec statements where
      _exec [] = return ()
      _exec (stmt:stmts) = do
           case stmt of
                DeclStmt d -> do
                         env <- iDecl d
                         local (\x -> env) (_exec stmts)
                _ -> do
                  iStmt stmt
                  _exec stmts

iStmt EmptyStmt = return()

iStmt (ExprStmt expr) = do
      iExpr expr
      return()

iStmt (IfStmt cond iftrue iffalse) = do
      condResult <- iExpr cond
      let elseStmt = (case iffalse of
               ElseEmpty -> return()
               Else stmt -> iStmt stmt) in
               if (isTruthy condResult) then (iStmt iftrue) else return()

iStmt while@(WhileStmt cond stmt) = do
      condResult <- iExpr cond
      if (isTruthy condResult) then (do
         iStmt stmt
         iStmt while) else return()

iExpr (ParenExpr e) = iExpr e

iExpr (EvalExpr qi) = iRetrQ (Sys.qIdentResolve qi)

iExpr (AssignExpr qi expr) = do
      val <- iExpr expr
      iAssignQ (Sys.qIdentResolve qi) val
      return val

iExpr (LiteralExpr l) = case l of
      IntLiteral i -> return $ IntVal i
      StringLiteral s -> return $ StringVal s
      TrueLiteral -> return $ BoolVal True
      FalseLiteral -> return $ BoolVal False
      UndefinedLiteral -> return $ UndefinedVal
      ObjectLiteral list -> do
                    objmap <- parseObjectLiteral list
                    return $ ObjectVal objmap

iExpr (PlusExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ v1 + v2

iExpr (MinusExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ v1 - v2

iExpr (TimesExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ v1 * v2

iExpr (EqExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ BoolVal $ v1 == v2

iExpr (NeqExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ BoolVal $ v1 /= v2

iExpr (LOrExpr e1 e2) = do
      v1 <- iExpr e1
      if (isTruthy v1) then return v1 else do
         v2 <- iExpr e2
         if isTruthy v2 then return v2 else return $ BoolVal False

iExpr (LAndrExpr e1 e2) = do
      v1 <- iExpr e1
      if (not $ isTruthy v1) then return v1 else do
         v2 <- iExpr e2
         if isTruthy v2 then return v2 else return $ BoolVal False

iExpr (LessExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ BoolVal $ v1 < v2

iExpr (GreaterExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ BoolVal $ v1 > v2

iExpr (LeqExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ BoolVal $ v1 <= v2

iExpr (GeqExpr e1 e2) = do
      v1 <- iExpr e1
      v2 <- iExpr e2
      return $ BoolVal $ v1 >= v2

parseObjectLiteral list = _parseObjectLiteral list M.empty where
_parseObjectLiteral [] m = return m
_parseObjectLiteral ((KVP key valExp):ls) m = do
                    Sys.print "dupa"
                    val <- iExpr valExp
                    _parseObjectLiteral ls (M.insert (keyS key) val m)
                where keyS key = case key of
                           KeyIdent (Ident str) -> str
                           KeyString str -> str

