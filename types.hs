module Types where

import qualified Data.Map as M
import Data.List
import Absmyjs

data Val = UndefinedVal | StringVal String | IntVal Integer | BoolVal Bool | ObjectVal (M.Map String Val) | FunctionVal [String] Stmt

instance Show Val where
         show (StringVal s) = s
         show (IntVal i) = show i
         show (BoolVal True) = "true"
         show (BoolVal False) = "false"
         show UndefinedVal = "undefined"
         show (ObjectVal om) = "{" ++ (intercalate ", " list) ++ "}"
              where list = M.foldWithKey (\key value result -> ("\"" ++ key ++ "\": " ++ (stringify value)):result) [] om
         show (FunctionVal args _) = "[function(" ++ fargs ++ ")]"
              where fargs = intercalate ", " args 

stringify :: Val -> String
stringify (StringVal s) = "\"" ++ s ++ "\""
stringify val = show val

instance Num Val where
         (+) (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
         (+) a1 a2 = StringVal $ (show a1) ++ (show a2)
         (*) (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
         (*) a1 a2 = StringVal "NaN"
         negate (IntVal i) = IntVal $ -i
         negate _ = StringVal "NaN"
         abs (IntVal i) = IntVal $ abs i
         abs _ = StringVal "NaN"
         fromInteger i = IntVal i
         signum (IntVal i) = IntVal $ signum i
         signum (StringVal s) = case s of
                "" -> 0
                _ -> 1
         signum (BoolVal b) = case b of
                False -> 0
                True -> 1
         signum (ObjectVal _) = 1
         signum UndefinedVal = 0
         signum (FunctionVal _ _) = 1

--instance Fractional Val where
         -- (/) (IntVal i1) (IntVal 0) = StringVal "NaN"
--         (/) (IntVal i1) (IntVal i2) = IntVal (i1 / i2)

instance Ord Val where
         (<) (IntVal i1) (IntVal i2) = i1 < i2
         (<) (StringVal i1) (StringVal i2) = i1 < i2
         (<) (BoolVal b1) (BoolVal b2) = b1 < b2
         (<) a1 a2 = False
         
         (<=) (IntVal i1) (IntVal i2) = i1 <= i2
         (<=) (StringVal i1) (StringVal i2) = i1 <= i2
         (<=) (BoolVal b1) (BoolVal b2) = b1 <= b2
         (<=) a1 a2 = False

instance Eq Val where
         (==) (IntVal i1) (IntVal i2) = i1 == i2
         (==) (StringVal i1) (StringVal i2) = i1 == i2
         (==) (BoolVal i1) (BoolVal i2) = i1 == i2
         (==) UndefinedVal UndefinedVal = True
         (==) (ObjectVal i1) (ObjectVal i2) = i1 == i2
         (==) a1 a2 = False

isTruthy v = case signum v of
         IntVal 0 -> False
         _ -> True