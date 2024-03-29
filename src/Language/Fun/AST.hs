module Language.Fun.AST ( TopLevel(..)
                        , Stmt(..)
                        , Expr(..)
                        ) where

data TopLevel imp def stmt
  = TopLevelImport imp
  | TopLevelDef def
  | TopLevelStmt stmt
  deriving (Eq, Show)

data Stmt sym expr stmt
  = StmtExpr expr
  | StmtLet sym expr
  | StmtIf expr [stmt] [stmt]
  | StmtRaise expr
  | StmtBeginRescue [stmt] sym [stmt]
  deriving (Eq, Show)

data Expr sym op lit expr
  = ExprLit lit
  | ExprVar sym
  | ExprFuncall expr [expr]
  | ExprIndex expr expr
  | ExprTernary expr expr expr
  | ExprParen expr
  | ExprNot expr
  | ExprBinOp expr op expr
  | ExprDebugger
  deriving (Eq, Show)

instance Functor (Stmt sym expr) where
  fmap f =
    let go (StmtExpr expr) = StmtExpr expr
        go (StmtLet sym expr) = StmtLet sym expr
        go (StmtIf cond andThen orElse) = StmtIf cond (map f andThen) (map f orElse)
        go (StmtRaise expr) = StmtRaise expr
        go (StmtBeginRescue body var onErr) = StmtBeginRescue (map f body) var (map f onErr)
    in  go

instance Functor (Expr sym op lit) where
  fmap f =
    let go (ExprLit lit) = ExprLit lit
        go (ExprVar sym) = ExprVar sym
        go (ExprFuncall g args) = ExprFuncall (f g) (map f args)
        go (ExprIndex xs idx) = ExprIndex (f xs) (f idx)
        go (ExprTernary cond andThen orElse) = ExprTernary (f cond) (f andThen) (f orElse)
        go (ExprParen expr) = ExprParen (f expr)
        go (ExprNot expr) = ExprNot (f expr)
        go (ExprBinOp expr op expr') = ExprBinOp (f expr) op (f expr')
        go ExprDebugger = ExprDebugger
    in  go
