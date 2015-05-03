-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parmyjs where
import Absmyjs
import Lexmyjs
import ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token
  '!' { PT _ (TS _ 1) }
  '!==' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&' { PT _ (TS _ 4) }
  '&&' { PT _ (TS _ 5) }
  '(' { PT _ (TS _ 6) }
  ')' { PT _ (TS _ 7) }
  '*' { PT _ (TS _ 8) }
  '+' { PT _ (TS _ 9) }
  '++' { PT _ (TS _ 10) }
  ',' { PT _ (TS _ 11) }
  '-' { PT _ (TS _ 12) }
  '--' { PT _ (TS _ 13) }
  '.' { PT _ (TS _ 14) }
  '/' { PT _ (TS _ 15) }
  ':' { PT _ (TS _ 16) }
  ';' { PT _ (TS _ 17) }
  '<' { PT _ (TS _ 18) }
  '<<' { PT _ (TS _ 19) }
  '<=' { PT _ (TS _ 20) }
  '=' { PT _ (TS _ 21) }
  '===' { PT _ (TS _ 22) }
  '>' { PT _ (TS _ 23) }
  '>=' { PT _ (TS _ 24) }
  '>>' { PT _ (TS _ 25) }
  '?' { PT _ (TS _ 26) }
  '[' { PT _ (TS _ 27) }
  ']' { PT _ (TS _ 28) }
  '^' { PT _ (TS _ 29) }
  'catch' { PT _ (TS _ 30) }
  'else' { PT _ (TS _ 31) }
  'false' { PT _ (TS _ 32) }
  'function' { PT _ (TS _ 33) }
  'if' { PT _ (TS _ 34) }
  'return' { PT _ (TS _ 35) }
  'return;' { PT _ (TS _ 36) }
  'throw' { PT _ (TS _ 37) }
  'true' { PT _ (TS _ 38) }
  'try' { PT _ (TS _ 39) }
  'typeof' { PT _ (TS _ 40) }
  'undefined' { PT _ (TS _ 41) }
  'var' { PT _ (TS _ 42) }
  'while' { PT _ (TS _ 43) }
  '{' { PT _ (TS _ 44) }
  '|' { PT _ (TS _ 45) }
  '||' { PT _ (TS _ 46) }
  '}' { PT _ (TS _ 47) }
  '~' { PT _ (TS _ 48) }

L_ident  { PT _ (TV $$) }
L_quoted { PT _ (TL $$) }
L_integ  { PT _ (TI $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
String  :: { String }  : L_quoted {  $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Program :: { Program }
Program : ListStmt { Progr $1 } 


ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } 
  | Stmt { (:[]) $1 }
  | Stmt ListStmt { (:) $1 $2 }


MaybeIdent :: { MaybeIdent }
MaybeIdent : {- empty -} { NoIdent } 
  | Ident { JustIdent $1 }


Decl :: { Decl }
Decl : 'var' Ident ';' { VarDecl $2 } 
  | 'var' Ident '=' Expr ';' { VarDeclAssign $2 $4 }
  | FunExpr { FunDecl $1 }


CompoundStmt :: { CompoundStmt }
CompoundStmt : '{' ListStmt '}' { CS $2 } 


Stmt :: { Stmt }
Stmt : CompoundStmt { CSS $1 } 
  | Expr ';' { ExprStmt $1 }
  | Decl { DeclStmt $1 }
  | ';' { EmptyStmt }
  | 'if' '(' Expr ')' Stmt ElseClause { IfStmt $3 $5 $6 }
  | 'while' '(' Expr ')' Stmt { WhileStmt $3 $5 }
  | 'throw' Expr { ThrowStmt $2 }
  | 'try' CompoundStmt 'catch' '(' Ident ')' CompoundStmt { TryCatchStmt $2 $5 $7 }
  | 'return' Expr ';' { ReturnStmt $2 }
  | 'return;' { EmptyReturnStmt }


Lvalue :: { Lvalue }
Lvalue : ListQIdentPart { QIdent $1 } 


QIdentPart :: { QIdentPart }
QIdentPart : IIdent { IdentPart $1 } 


ListQIdentPart :: { [QIdentPart] }
ListQIdentPart : QIdentPart { (:[]) $1 } 
  | QIdentPart '.' ListQIdentPart { (:) $1 $3 }


IIdent :: { IIdent }
IIdent : Ident { IIdentBare $1 } 
  | Ident '[' Expr ']' { IIdentIndexed $1 $3 }


ParamNames :: { ParamNames }
ParamNames : '(' ListIdent ')' { ParamNameList $2 } 


ListIdent :: { [Ident] }
ListIdent : {- empty -} { [] } 
  | Ident { (:[]) $1 }
  | Ident ',' ListIdent { (:) $1 $3 }


Params :: { Params }
Params : '(' ListParam ')' { ParamList $2 } 


Param :: { Param }
Param : Expr { ParamExpr $1 } 


ListParam :: { [Param] }
ListParam : {- empty -} { [] } 
  | Param { (:[]) $1 }
  | Param ',' ListParam { (:) $1 $3 }


KeyValuePair :: { KeyValuePair }
KeyValuePair : Key ':' Expr { KVP $1 $3 } 


Key :: { Key }
Key : Ident { KeyIdent $1 } 
  | String { KeyString $1 }


ListKeyValuePair :: { [KeyValuePair] }
ListKeyValuePair : {- empty -} { [] } 
  | KeyValuePair { (:[]) $1 }
  | KeyValuePair ',' ListKeyValuePair { (:) $1 $3 }


Literal :: { Literal }
Literal : Integer { IntLiteral $1 } 
  | String { StringLiteral $1 }
  | 'true' { TrueLiteral }
  | 'false' { FalseLiteral }
  | 'undefined' { UndefinedLiteral }
  | '{' ListKeyValuePair '}' { ObjectLiteral $2 }
  | '[' ListParam ']' { ArrayLiteral $2 }


FunExpr :: { FunExpr }
FunExpr : 'function' MaybeIdent ParamNames CompoundStmt { Fun $2 $3 $4 } 


Expr :: { Expr }
Expr : FunExpr { FunExpression $1 } 
  | Lvalue '=' Expr { AssignExpr $1 $3 }
  | Expr1 { $1 }


Expr3 :: { Expr }
Expr3 : Expr4 '?' Expr ':' Expr3 { CondExpr $1 $3 $5 } 
  | Expr4 { $1 }


Expr4 :: { Expr }
Expr4 : Expr4 '||' Expr5 { LOrExpr $1 $3 } 
  | Expr5 { $1 }


Expr5 :: { Expr }
Expr5 : Expr5 '&&' Expr6 { LAndrExpr $1 $3 } 
  | Expr6 { $1 }


Expr6 :: { Expr }
Expr6 : Expr6 '|' Expr7 { BitOrExpr $1 $3 } 
  | Expr7 { $1 }


Expr7 :: { Expr }
Expr7 : Expr7 '^' Expr8 { BitXorExpr $1 $3 } 
  | Expr8 { $1 }


Expr8 :: { Expr }
Expr8 : Expr8 '&' Expr9 { BitAndExpr $1 $3 } 
  | Expr9 { $1 }


Expr9 :: { Expr }
Expr9 : Expr9 '===' Expr10 { EqExpr $1 $3 } 
  | Expr9 '!==' Expr10 { NeqExpr $1 $3 }
  | Expr10 { $1 }


Expr10 :: { Expr }
Expr10 : Expr10 '<' Expr11 { LessExpr $1 $3 } 
  | Expr10 '>' Expr11 { GreaterExpr $1 $3 }
  | Expr10 '<=' Expr11 { LeqExpr $1 $3 }
  | Expr10 '>=' Expr11 { GeqExpr $1 $3 }
  | Expr11 { $1 }


Expr11 :: { Expr }
Expr11 : Expr11 '<<' Expr12 { ShlExpr $1 $3 } 
  | Expr12 { $1 }


Expr12 :: { Expr }
Expr12 : Expr12 '>>' Expr13 { ShrExpr $1 $3 } 
  | Expr12 '+' Expr13 { PlusExpr $1 $3 }
  | Expr12 '-' Expr13 { MinusExpr $1 $3 }
  | Expr13 { $1 }


Expr13 :: { Expr }
Expr13 : Expr13 '*' Expr14 { TimesExpr $1 $3 } 
  | Expr13 '/' Expr14 { DivExpr $1 $3 }
  | Expr13 '%' Expr14 { ModExpr $1 $3 }
  | Expr14 { $1 }


Expr14 :: { Expr }
Expr14 : '++' Expr15 { PreincExpr $2 } 
  | '--' Expr15 { PredecExpr $2 }
  | UnaryOp Expr15 { PreopExpr $1 $2 }
  | 'typeof' Expr15 { TypeofExpr $2 }
  | Expr15 { $1 }


Expr15 :: { Expr }
Expr15 : '(' Expr ')' { ParenExpr $2 } 
  | Expr16 { $1 }


Expr16 :: { Expr }
Expr16 : Lvalue Params { CallExpr $1 $2 } 
  | Expr17 { $1 }


Expr17 :: { Expr }
Expr17 : Literal { LiteralExpr $1 } 
  | Expr18 { $1 }


Expr18 :: { Expr }
Expr18 : Lvalue { EvalExpr $1 } 
  | '(' Expr ')' { $2 }


UnaryOp :: { UnaryOp }
UnaryOp : '!' { NegOp } 
  | '~' { TildeOp }


ElseClause :: { ElseClause }
ElseClause : 'else' Stmt { Else $2 } 
  | {- empty -} { ElseEmpty }


Expr1 :: { Expr }
Expr1 : Expr2 { $1 } 


Expr2 :: { Expr }
Expr2 : Expr3 { $1 } 



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

