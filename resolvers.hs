module Resolvers where



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
