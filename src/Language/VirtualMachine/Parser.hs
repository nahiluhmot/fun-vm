{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Language.VirtualMachine.Parser ( ParseInput
                                      , ParseStream
                                      , ParseTopLevel
                                      , ParseStmt
                                      , ParseExpr
                                      , ParseLit
                                      , ParseFunction
                                      , ParseMap
                                      , runParser
                                      ) where

import Data.Functor.Identity (Identity)

import Data.Functor (($>))
import Data.Text (Text)
import Text.Parsec (ParseError, SourceName, SourcePos)
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (runParser)
import qualified Text.Parsec.Prim as P

import Language.VirtualMachine.Data.AST (TopLevel(..), Stmt(..), Expr(..), LitExpr(..))
import Language.VirtualMachine.Data.Fix (Fix(..))
import Language.VirtualMachine.Data.Value (Value(..), Function(..))
import Language.VirtualMachine.Lexer (LexToken, LexOp, Tok(..), TokOp(..), TokLit(..), TokGroupOp(..), TokBinOp(..), TokSpecialOp(..))

type ParseInput = (SourcePos, LexToken)
type ParseStream s = Stream s Identity ParseInput
type Parser a = forall s. ParseStream s => ParsecT s () Identity a

type ParseTopLevel = TopLevel Text Text ParseExpr ParseStmt
type ParseStmt = Fix (Stmt Text ParseExpr)
type ParseExpr = LitExpr (Expr Text TokBinOp) ParseLit
type ParseLit = Value Text Integer Rational Text ParseFunction [] ParseMap
newtype ParseFunction = ParseFunction { runParseFunction :: Function [Text] [ParseStmt] }
newtype ParseMap a = ParseMap { runParseMap :: [(ParseExpr, a)] }

runParser :: ParseStream s => SourceName -> s -> Either ParseError [ParseTopLevel]
runParser =
  P.runParser topLevel ()

topLevel :: Parser [ParseTopLevel]
topLevel =
  (++) <$> many (uncurry TopLevelImport <$> topLevelImport)
       <*> many (fmap (uncurry TopLevelDef) topLevelDef <|>
                 fmap TopLevelStmt stmt)

topLevelImport :: Parser (Text, Text)
topLevelImport =
  (,) <$> (symEq "import" *> litUnreservedSymbol <* symEq "from")
      <*> (litString <* specialOp TokSemiColon)

topLevelDef :: Parser (Text, ParseExpr)
topLevelDef =
  (,) <$> (symEq "def" *> litUnreservedSymbol)
      <*> (expr <* specialOp TokSemiColon)

stmt :: Parser ParseStmt
stmt = Fix . uncurry StmtAssign <$> stmtAssign
   <|> Fix . uncurry3 StmtIf <$> stmtIf
   <|> Fix . StmtExpr <$> stmtExpr

stmtIf :: Parser (ParseExpr, [ParseStmt], [ParseStmt])
stmtIf =
  let cond = symEq "if" *> expr
      andThen = groupOp TokOpenBracket *> many stmt <* groupOp TokCloseBracket
      orElse = symEq "else" *> andThen
      elseIf = symEq "else" *> fmap (return . Fix . uncurry3 StmtIf) stmtIf
  in  tup3 <$> cond <*> andThen <*> (try orElse <|> elseIf <|> pure [])

stmtAssign :: Parser (Text, ParseExpr)
stmtAssign =
  (,) <$> (symEq "let" *> litUnreservedSymbol <* specialOp TokAssign)
      <*> (expr <* specialOp TokSemiColon)

stmtExpr :: Parser ParseExpr
stmtExpr =
  expr <* specialOp TokSemiColon

expr :: Parser ParseExpr
expr =
  let wrap = LitExpr . Fix
      unwrap = runLitExpr
      unwrap2 (a, b) = (unwrap a, unwrap b)
      unwrap3 (a, b, c) = (unwrap a, unwrap b, unwrap c)
      unwrapFuncall (f, args) = (unwrap f, map unwrap args)
      unwrapBinOp (left, binOp, right) = (unwrap left, binOp, unwrap right)
  in  wrap . uncurry ExprFuncall . unwrapFuncall <$> try exprFuncall
  <|> wrap . uncurry ExprIndex . unwrap2 <$> try exprIndex
  <|> wrap . uncurry3 ExprTernary . unwrap3 <$> try exprTernary
  <|> wrap ExprDebugger <$ exprDebugger
  <|> wrap . ExprVar <$> litUnreservedSymbol
  <|> wrap . ExprLit <$> exprLit
  <|> wrap . ExprParen . unwrap <$> exprParen
  <|> wrap . ExprNot . unwrap <$> exprNot
  <|> wrap . uncurry3 ExprBinOp . unwrapBinOp <$> exprBinOp

exprParen :: Parser ParseExpr
exprParen =
  groupOp TokOpenParen *> expr <* groupOp TokCloseParen

exprBinOp :: Parser (ParseExpr, TokBinOp, ParseExpr)
exprBinOp =
  tup3 <$> expr <*> binOpAny <*> expr

exprNot :: Parser ParseExpr
exprNot =
  specialOp TokNot *> expr

exprTernary :: Parser (ParseExpr, ParseExpr, ParseExpr)
exprTernary =
  tup3 <$> (expr <* specialOp TokQuestionMark)
       <*> expr
       <*> (specialOp TokColon *> expr)

exprIndex :: Parser (ParseExpr, ParseExpr)
exprIndex =
  let dynamicIdx = between (groupOp TokOpenBrace) (groupOp TokCloseBrace) expr
      staticIdx = specialOp TokDot *> fmap (LitExpr . Fix . ExprLit . Sym) litUnquotedSymbol
  in  (,) <$> expr <*> (dynamicIdx <|> staticIdx)  <?> "index operator"

exprDebugger :: Parser ()
exprDebugger =
  symEq "debugger" $> ()

exprFuncall :: Parser (ParseExpr, [ParseExpr])
exprFuncall =
  let func = expr
      args = list TokOpenParen TokCloseParen TokComma expr
  in  (,) <$> func <*> args <?> "function call"

exprLit :: Parser (ParseLit ParseExpr)
exprLit = Nil <$ litNil
      <|> Sym <$> litQuotedSymbol
      <|> Str <$> litString
      <|> Float <$> litNum
      <|> Vector <$> litVec
      <|> Map . ParseMap <$> litMap
      <|> Func <$> litFunction

litFunction :: Parser ParseFunction
litFunction =
  let args = singleArg <|> multipleArgs
      singleArg = fmap (\x -> [x]) litUnreservedSymbol
      multipleArgs = list TokOpenParen TokCloseParen TokComma litUnreservedSymbol
      body = exprBody <|> stmtBody
      exprBody =  fmap (\x -> [Fix $ StmtExpr x]) expr
      stmtBody = between (groupOp TokOpenBracket) (groupOp TokCloseBracket) (many stmt)
  in  ParseFunction <$> (Function <$> args <*> body)

litMap :: Parser [(ParseExpr, ParseExpr)]
litMap =
  let key :: Parser ParseExpr
      key = try (LitExpr . Fix . ExprLit . Sym <$> keyLit) <|> keyExpr
      keyLit :: Parser Text
      keyLit = (litUnquotedSymbol <|> litString) <* specialOp TokColon
      keyExpr :: Parser ParseExpr
      keyExpr = expr <* specialOp TokArrow
  in  list TokOpenBracket TokCloseBracket TokComma ((,) <$> key <*> expr) <?> "map literal"

litVec :: Parser [ParseExpr]
litVec =
  list TokOpenBrace TokCloseBrace TokComma expr <?> "vector literal"

litQuotedSymbol :: Parser Text
litQuotedSymbol =
  specialOp TokColon *> (litUnquotedSymbol <|> litString) <?> "quoted symbol"

litUnreservedSymbol :: Parser Text
litUnreservedSymbol =
  let test (TokLit (TokSym s))
        | elem s reservedWords = Nothing
        | otherwise = Just s
      test _ = Nothing
  in  satisfyMaybe test <?> "unreserved symbol"

symEq :: Text -> Parser Text
symEq s =
  let test (TokLit (TokSym s'))
        | s == s' = Just s
        | otherwise = Nothing
      test _ = Nothing
  in  satisfyMaybe test <?> "symbol: " ++ show s

litUnquotedSymbol :: Parser Text
litUnquotedSymbol =
  let test (TokLit (TokSym str)) = Just str
      test _ = Nothing
  in  satisfyMaybe test <?> "unquoted symbol"

litNum :: Parser Rational
litNum =
  let test (TokLit (TokNum n)) = Just n
      test _ = Nothing
  in  satisfyMaybe test <?> "number"

litString :: Parser Text
litString =
  let test (TokLit (TokStr str)) = Just str
      test _ = Nothing
  in  satisfyMaybe test <?> "string literal"

litNil :: Parser ()
litNil =
  symEq "nil" $> ()

groupOp :: TokGroupOp -> Parser TokGroupOp
groupOp o =
  let test (TokGroupOp o')
        | o == o' = Just o
        | otherwise = Nothing
      test _ = Nothing
  in  opMaybe test <?> "group operator: " ++ show o

specialOp :: TokSpecialOp -> Parser TokSpecialOp
specialOp o =
  let test (TokSpecialOp o')
        | o == o' = Just o
        | otherwise = Nothing
      test _ = Nothing
  in  opMaybe test <?> "special operator: " ++ show o

binOpAny :: Parser TokBinOp
binOpAny =
  let test (TokBinOp o) = Just o
      test _ = Nothing
  in  opMaybe test <?> "binary operator"

op :: LexOp -> Parser LexOp
op o =
  opSatisfy (== o) <?> "operator: " ++ show o

opSatisfy :: (LexOp -> Bool)  -> Parser LexOp
opSatisfy f =
  let test o
        | f o = Just o
        | otherwise = Nothing
  in  opMaybe test

opMaybe :: (LexOp -> Maybe a) -> Parser a
opMaybe f =
  let test (TokOp tok) = f tok
      test _ = Nothing
  in  satisfyMaybe test

list :: TokGroupOp
            -> TokGroupOp
            -> TokSpecialOp
            -> Parser a
            -> Parser [a]
list begin end sep ele =
  op (TokGroupOp begin) *> sepBy ele (op (TokSpecialOp sep)) <* op (TokGroupOp end)

satisfy :: (Show a, Stream s m (SourcePos, a))
        => (a -> Bool)
        -> ParsecT s u m a
satisfy f =
  let test a
        | f a = Just a
        | otherwise = Nothing
  in  satisfyMaybe test

satisfyMaybe :: (Show a, Stream s m (SourcePos, a))
             => (a -> Maybe b)
             -> ParsecT s u m b
satisfyMaybe f =
  let nextTok _pos (pos, _tok) _stream = pos
  in  tokenPrim (show . snd) nextTok (f . snd)

reservedWords :: [Text]
reservedWords =
  [ "def"
  , "else"
  , "from"
  , "if"
  , "let"
  , "module"
  , "nil"
  ]

tup3 :: a -> b -> c -> (a, b, c)
tup3 a b c = (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
