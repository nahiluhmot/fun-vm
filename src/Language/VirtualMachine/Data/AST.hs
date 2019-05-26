module Language.VirtualMachine.Data.AST ( TopLevel(..)
                                        , Stmt(..)
                                        , LitExpr(..)
                                        , Expr(..)
                                        ) where

import Language.VirtualMachine.Data.Fix (Fix)

data TopLevel sym str expr stmt
  = TopLevelModule sym
  | TopLevelImport sym str
  | TopLevelDef sym expr
  | TopLevelStmt stmt

data Stmt sym expr stmt
  = StmtExpr expr
  | StmtAssign sym expr
  | StmtIf expr [stmt] [stmt]

newtype LitExpr expr lit
  = LitExpr { runLitExpr :: Fix (expr (lit (LitExpr expr lit))) }

data Expr sym lit expr
  = ExprLit lit
  | ExprVar sym
  | ExprFuncall expr [expr]
  | ExprIndex expr expr
  | ExprTernary expr expr expr
  | ExprDebugger
