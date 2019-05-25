module Language.VirtualMachine.Data.AST ( TopLevel(..)
                                        , FnStmt(..)
                                        , Stmt(..)
                                        , Expr(..)
                                        , Literal
                                        ) where

import Language.VirtualMachine.Data.Fix (Fix)
import Language.VirtualMachine.Data.Value (ValueF)

data TopLevel sym str expr stmt
  = TopLevelModule sym
  | TopLevelImport sym str
  | TopLevelDef sym expr
  | TopLevelStmt stmt

data Stmt sym expr stmt
  = StmtExpr expr
  | StmtAssign sym expr
  | StmtIf expr [stmt] [stmt]

data FnStmt expr stmt
  = FnStmt stmt
  | FnStmtRecur [expr]

data Expr sym vec lit expr
  = ExprLit lit
  | ExprVar sym
  | ExprFuncall expr (vec expr)
  | ExprIndex expr (vec expr)
  | ExprTernary expr expr expr
  | ExprDebugger

type Literal sym int float string func seq intMap
  = Fix (ValueF sym int float string func seq intMap)
