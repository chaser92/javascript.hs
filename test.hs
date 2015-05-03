import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.State

ab [] = modify (+1)

ab (h:t) = do
   env <- lift ask
   if h==6 then throwError "ddd" else return()
   modify (+env)
   ab t

s = runState (runReaderT (runErrorT (ab [1,2,3,4,5])) 14) 15