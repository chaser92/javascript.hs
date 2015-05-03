module Skelmyjs where

-- Haskell module generated by the BNF converter

import Absmyjs
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Progr stmts  -> failure x


transMaybeIdent :: MaybeIdent -> Result
transMaybeIdent x = case x of
  NoIdent  -> failure x
  JustIdent id  -> failure x


transDecl :: Decl -> Result
transDecl x = case x of
  VarDecl id  -> failure x
  VarDeclAssign id expr  -> failure x
  FunDecl funexpr  -> failure x


transCompoundStmt :: CompoundStmt -> Result
transCompoundStmt x = case x of
  CS stmts  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  CSS compoundstmt  -> failure x
  ExprStmt expr  -> failure x
  DeclStmt decl  -> failure x
  EmptyStmt  -> failure x
  IfStmt expr stmt elseclause  -> failure x
  WhileStmt expr stmt  -> failure x
  ThrowStmt expr  -> failure x
  TryCatchStmt compoundstmt1 id2 compoundstmt3  -> failure x
  ReturnStmt expr  -> failure x
  EmptyReturnStmt  -> failure x


transLvalue :: Lvalue -> Result
transLvalue x = case x of
  QIdent qidentparts  -> failure x


transQIdentPart :: QIdentPart -> Result
transQIdentPart x = case x of
  IdentPart iident  -> failure x


transIIdent :: IIdent -> Result
transIIdent x = case x of
  IIdentBare id  -> failure x
  IIdentIndexed id expr  -> failure x


transParamNames :: ParamNames -> Result
transParamNames x = case x of
  ParamNameList ids  -> failure x


transParams :: Params -> Result
transParams x = case x of
  ParamList params  -> failure x


transParam :: Param -> Result
transParam x = case x of
  ParamExpr expr  -> failure x


transKeyValuePair :: KeyValuePair -> Result
transKeyValuePair x = case x of
  KVP key expr  -> failure x


transKey :: Key -> Result
transKey x = case x of
  KeyIdent id  -> failure x
  KeyString str  -> failure x


transLiteral :: Literal -> Result
transLiteral x = case x of
  IntLiteral n  -> failure x
  StringLiteral str  -> failure x
  TrueLiteral  -> failure x
  FalseLiteral  -> failure x
  UndefinedLiteral  -> failure x
  ObjectLiteral keyvaluepairs  -> failure x
  ArrayLiteral params  -> failure x


transFunExpr :: FunExpr -> Result
transFunExpr x = case x of
  Fun maybeident paramnames compoundstmt  -> failure x


transExpr :: Expr -> Result
transExpr x = case x of
  FunExpression funexpr  -> failure x
  AssignExpr lvalue expr  -> failure x
  CondExpr expr1 expr2 expr3  -> failure x
  LOrExpr expr1 expr2  -> failure x
  LAndrExpr expr1 expr2  -> failure x
  BitOrExpr expr1 expr2  -> failure x
  BitXorExpr expr1 expr2  -> failure x
  BitAndExpr expr1 expr2  -> failure x
  EqExpr expr1 expr2  -> failure x
  NeqExpr expr1 expr2  -> failure x
  LessExpr expr1 expr2  -> failure x
  GreaterExpr expr1 expr2  -> failure x
  LeqExpr expr1 expr2  -> failure x
  GeqExpr expr1 expr2  -> failure x
  ShlExpr expr1 expr2  -> failure x
  ShrExpr expr1 expr2  -> failure x
  PlusExpr expr1 expr2  -> failure x
  MinusExpr expr1 expr2  -> failure x
  TimesExpr expr1 expr2  -> failure x
  DivExpr expr1 expr2  -> failure x
  ModExpr expr1 expr2  -> failure x
  PreincExpr expr  -> failure x
  PredecExpr expr  -> failure x
  PreopExpr unaryop expr  -> failure x
  TypeofExpr expr  -> failure x
  ParenExpr expr  -> failure x
  CallExpr lvalue params  -> failure x
  LiteralExpr literal  -> failure x
  EvalExpr lvalue  -> failure x


transUnaryOp :: UnaryOp -> Result
transUnaryOp x = case x of
  NegOp  -> failure x
  TildeOp  -> failure x


transElseClause :: ElseClause -> Result
transElseClause x = case x of
  Else stmt  -> failure x
  ElseEmpty  -> failure x



