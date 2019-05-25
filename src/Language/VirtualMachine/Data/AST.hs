module Language.VirtualMachine.Data.AST ( TopLevel(..)
                                        , FnStmt(..)
                                        , Stmt(..)
                                        , Expr(..)
                                        , Literal
                                        ) where

import Language.VirtualMachine.Data.Fix (Fix)
import Language.VirtualMachine.Data.Value (Value)

data TopLevel sym str expr stmt
  = TopLevelModule sym
  | TopLevelImport sym str
  | TopLevelDef sym expr
  | TopLevelStmt stmt

data Stmt sym vec expr stmt
  = StmtExpr expr
  | StmtAssign sym expr
  | StmtIf expr (vec stmt) (vec stmt)

data FnStmt vec expr stmt
  = FnStmt stmt
  | FnStmtRecur (vec expr)

data Expr sym vec lit expr
  = ExprLit lit
  | ExprVar sym
  | ExprFuncall expr (vec expr)
  | ExprIndex expr (vec expr)
  | ExprTernary expr expr expr
  | ExprDebugger

type Literal sym int float string func seq intMap
  = Fix (Value sym int float string func seq intMap)
