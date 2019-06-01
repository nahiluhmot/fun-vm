{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.VirtualMachine.Compiler ( CompilerInsn
                                        , CompilerValue
                                        , compile
                                        ) where

import Prelude hiding (mod)

import Control.Monad (sequence_)
import Data.Void (Void)

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Text (Text)

import Language.VirtualMachine.Data.Fix (cata)
import Language.VirtualMachine.Data.AST (TopLevel(..), Stmt(..), Expr(..))
import Language.VirtualMachine.Data.Value (Value(..))
import Language.VirtualMachine.Data.Instruction (Instruction(..))
import Language.VirtualMachine.Data.Void (VoidF)
import Language.VirtualMachine.Lexer (TokBinOp(..))
import Language.VirtualMachine.Parser

type CompilerInsn = Instruction Text Int CompilerValue
type CompilerValue = Value Text Rational Text Void VoidF VoidF Void

type Compiler = CompilerM ()
type CompilerM = Writer (Seq CompilerInsn)

compile :: Text -> [ParseTopLevel] -> Seq CompilerInsn
compile name topLevel =
  execWriter $ emit1 (Module name) *> sequence_ (map compileTopLevel topLevel)

compileTopLevel :: ParseTopLevel -> Compiler
compileTopLevel =
  let compileTopLevel' (TopLevelImport (_, sym, file)) =
        pushLit (Str file) *> emit1 (Import sym)
      compileTopLevel' (TopLevelDef (_, sym, expr)) =
        compileExpr expr *> emit1 (Def sym)
      compileTopLevel' (TopLevelStmt stmt) =
        compileStmt stmt
  in  compileTopLevel'

compileStmt :: ParseStmt -> Compiler
compileStmt =
  let compileStmt' :: Stmt Text ParseExpr Compiler -> Compiler
      compileStmt' (StmtExpr expr) = compileExpr expr
      compileStmt' (StmtLet sym expr) = compileExpr expr *> emit1 (Let sym)
      compileStmt' (StmtIf cond andThen orElse) =
        compileIfThenElse (compileExpr cond)
                          (sequence_ andThen)
                          (sequence_ orElse)
  in  cata (compileStmt' . snd . runParseStmtF)

compileExpr :: ParseExpr -> Compiler
compileExpr =
  let compileExpr' :: (Expr Text TokBinOp (ParseLitF Compiler) Compiler) -> Compiler
      compileExpr' (ExprLit lit) =
        compileLit lit
      compileExpr' (ExprVar sym) =
        emit1 $ Var sym
      compileExpr' (ExprFuncall func args) =
        func *> sequence_ args *> emit1 (Funcall (length args))
      compileExpr' (ExprIndex ele idx) =
        emit1 (Var "*index*") *> ele *> idx *> emit1 (Funcall 2)
      compileExpr' (ExprTernary cond andThen orElse) =
        compileIfThenElse cond andThen orElse
      compileExpr' (ExprParen inner) =
        inner
      compileExpr' (ExprNot inner) =
        emit1 (Var "*not*") *> inner *> emit1 (Funcall 1)
      compileExpr' (ExprBinOp left op right) =
        let compileOp TokPow = binFuncall "*pow*"
            compileOp TokDiv = binFuncall "*div*"
            compileOp TokMul = binFuncall "*mul*"
            compileOp TokMod = binFuncall "*mod*"
            compileOp TokPlus = binFuncall "*add*"
            compileOp TokMinus = binFuncall "*sub*"
            compileOp TokEq = binFuncall "*eq*"
            compileOp TokNeq = binFuncall "*neq*"
            compileOp TokGt = binFuncall "*gt*"
            compileOp TokGte = binFuncall "*gte*"
            compileOp TokLt = binFuncall "*lt*"
            compileOp TokLte = binFuncall "*lte*"
            compileOp TokAnd =
              compileIfThenElse (left *> emit1 Dup)
                                (emit1 Pop *> right)
                                (pure ())
            compileOp TokOr =
              compileIfThenElse (left *> emit1 Dup)
                                (pure ())
                                (emit1 Pop *> right)
            binFuncall sym = emit1 (Var sym) *> left *> right *> emit1 (Funcall 2)
        in  compileOp op
      compileExpr' ExprDebugger =
        emit1 Debugger
  in  cata (compileExpr' . runParseExprF)

compileIfThenElse :: Compiler -> Compiler -> Compiler -> Compiler
compileIfThenElse cond andThen orElse =
  let elseInsns = execWriter orElse
      branchIf = emit1 . BranchIf $ S.length elseInsns
  in  cond *> branchIf *> emit elseInsns *> andThen

compileLit :: ParseLitF Compiler -> Compiler
compileLit =
  let go :: ParseLitF Compiler -> Compiler
      go Nil = pushLit Nil
      go (Sym sym) = pushLit $ Sym sym
      go (Number num) = pushLit $ Number num
      go (Str str) = pushLit $ Str str
      go (Vector eles) =
        emit1 (Var "*vec*") *> sequence_ eles *> emit1 (Funcall (length eles))
      go (Map (ParseMap kvs)) =
        emit1 (Var "*map*") *> sequence_ (map (\(expr, val) -> compileExpr expr *> val) kvs) *> emit1 (Funcall (2 * (length kvs)))
      go (Func (args, body)) =
        let args' = S.fromList args
            argInsns = fmap (Push . Sym) args'
            lambdaInsns = [Push (Number len), Lambda (S.length args')]
            bodyInsns = execWriter . sequence_ $ map compileStmt body
            len = fromIntegral $ S.length bodyInsns
        in  emit $ argInsns <> lambdaInsns <> bodyInsns |> Return
  in  go

pushLit :: CompilerValue -> Compiler
pushLit = emit1 . Push

emit1 :: CompilerInsn -> Compiler
emit1 =
  emit . pure

emit :: Seq CompilerInsn -> Compiler
emit =
  tell
