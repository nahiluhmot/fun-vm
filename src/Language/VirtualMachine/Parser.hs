{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Language.VirtualMachine.Parser ( ParseInput
                                      , ParseStream
                                      , ParseTopLevel
                                      , ParseImport
                                      , ParseDef
                                      , ParseStmt
                                      , ParseStmtF(..)
                                      , ParseExpr
                                      , ParseLit
                                      , ParseFunction
                                      , ParseMap(..)
                                      , runParser
                                      ) where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Functor.Identity (Identity)

import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import Text.Parsec (ParseError, SourceName, SourcePos)
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (runParser)
import qualified Text.Parsec.Prim as P

import Language.VirtualMachine.Data.AST (TopLevel(..), Stmt(..), Expr(..), LitExpr(..))
import Language.VirtualMachine.Data.Fix (Fix(..), cata)
import Language.VirtualMachine.Data.Value (Value(..))
import Language.VirtualMachine.Lexer (LexToken, LexOp, Tok(..), TokOp(..), TokLit(..), TokGroupOp(..), TokBinOp(..), TokSpecialOp(..))

type ParseInput = (SourcePos, LexToken)
type ParseStream s = Stream s Identity ParseInput
type Parser a = forall s. ParseStream s => ParsecT s ParserState Identity a
-- For detecting recursive expressions
type ParserState = S.Set (SourcePos, RecExpr)
data RecExpr
  = RecFuncall
  | RecIndex
  | RecTernary
  | RecParen
  | RecBinOp
  deriving (Eq, Ord, Enum, Show)

type ParseTopLevel = TopLevel ParseImport ParseDef ParseStmt
-- pos, var, file
type ParseImport = (SourcePos, Text, Text)
-- pos, var, expression
type ParseDef = (SourcePos, Text, ParseExpr)
type ParseStmt = Fix ParseStmtF
newtype ParseStmtF a = ParseStmtF { runParseStmtF :: (SourcePos, Stmt Text ParseExpr a) } deriving (Show)
type ParseExpr = LitExpr (Expr Text TokBinOp) ParseLit
type ParseLit = Value Text Integer Rational Text ParseFunction [] ParseMap
-- args, body
type ParseFunction = ([Text], [ParseStmt])
newtype ParseMap a = ParseMap { runParseMap :: [(ParseExpr, a)] } deriving (Show)

runParser :: ParseStream s => SourceName -> s -> Either ParseError [ParseTopLevel]
runParser =
  P.runParser (topLevel <* eof) S.empty

topLevel :: Parser [ParseTopLevel]
topLevel =
  (++) <$> many (TopLevelImport <$> topLevelImport)
       <*> many (fmap TopLevelDef topLevelDef <|>
                 fmap TopLevelStmt stmt)

topLevelImport :: Parser (SourcePos, Text, Text)
topLevelImport =
  tup3 <$> getPosition
       <*> (symEq "import" *> litUnreservedSymbol <* symEq "from")
       <*> (litString <* specialOp TokSemiColon)
       <?> "import"

topLevelDef :: Parser (SourcePos, Text, ParseExpr)
topLevelDef =
  tup3 <$> getPosition
       <*> (symEq "def" *> litUnreservedSymbol)
       <*> (expr <* specialOp TokSemiColon)
       <?> "definition"

stmt :: Parser ParseStmt
stmt =
  let stmtWithoutPos :: Parser (Stmt Text ParseExpr ParseStmt)
      stmtWithoutPos = fmap (uncurry StmtAssign) stmtAssign
                   <|> fmap (uncurry3 StmtIf) stmtIf
                   <|> fmap StmtExpr stmtExpr
  in  stmtWithPos stmtWithoutPos

stmtWithPos :: Parser (Stmt Text ParseExpr ParseStmt) -> Parser ParseStmt
stmtWithPos action =
  let wrap pos a = Fix (ParseStmtF (pos, a))
  in  wrap <$> getPosition <*> action

stmtIf :: Parser (ParseExpr, [ParseStmt], [ParseStmt])
stmtIf =
  let cond = symEq "if" *> expr
      andThen = groupOp TokOpenBracket *> many stmt <* groupOp TokCloseBracket
      orElse = symEq "else" *> andThen
      elseIf = symEq "else" *> fmap return (stmtWithPos $ fmap (uncurry3 StmtIf) stmtIf)
  in  tup3 <$> cond <*> andThen <*> (try orElse <|> elseIf <|> pure []) <?> "if statement"

stmtAssign :: Parser (Text, ParseExpr)
stmtAssign =
  (,) <$> (symEq "let" *> litUnreservedSymbol <* specialOp TokAssign)
      <*> (expr <* specialOp TokSemiColon) <?> "assignment"

stmtExpr :: Parser ParseExpr
stmtExpr =
  expr <* specialOp TokSemiColon

expr :: Parser ParseExpr
expr =
  let parseFuncall =
        recLock RecFuncall $ uncurry ExprFuncall <$> exprFuncall
      parseIndex =
        recLock RecIndex $ uncurry ExprIndex <$> exprIndex
      parseTernary =
        recLock RecTernary $ uncurry3 ExprTernary <$> exprTernary
      parseBinOp =
        recLock RecBinOp $ uncurry3 ExprBinOp <$> exprBinOp
      parseParen =
        ExprParen <$> exprParen
      parseNot =
        ExprNot <$> exprNot
      parseVar =
        ExprVar <$> litUnreservedSymbol
      parseLit =
        ExprLit <$> exprLit
      parseDebugger =
        ExprDebugger <$ exprDebugger

      parseExpr = try parseFuncall
              <|> parseNot
              <|> try parseBinOp
              <|> try parseTernary
              <|> try parseIndex
              <|> try parseLit
              <|> parseDebugger
              <|> parseParen
              <|> parseVar

  in  LitExpr . Fix . fmap runLitExpr <$> parseExpr

recLock :: RecExpr -> Parser a -> Parser a
recLock recExpr child = do
  record@(_, pos) <- flip (,) recExpr <$> getPosition
  forbidden <- getState

  when (S.member record forbidden) $
     parserFail ("Recursive usage of " ++ show recExpr ++ " at " ++ show pos)

  putState (S.insert record forbidden) *> child <* putState forbidden

exprParen :: Parser ParseExpr
exprParen =
  groupOp TokOpenParen *> expr <* groupOp TokCloseParen <?> "parenthetical expression"

exprBinOp :: Parser (ParseExpr, TokBinOp, ParseExpr)
exprBinOp =
  tup3 <$> expr <*> binOpAny <*> expr <?> "binary operator"

exprNot :: Parser ParseExpr
exprNot =
  specialOp TokNot *> expr <?> "not operator"

exprTernary :: Parser (ParseExpr, ParseExpr, ParseExpr)
exprTernary =
  tup3 <$> (expr <* specialOp TokQuestionMark)
       <*> expr
       <*> (specialOp TokColon *> expr)
       <?> "ternary operator"

exprIndex :: Parser (ParseExpr, ParseExpr)
exprIndex =
  let dynamicIdx = between (groupOp TokOpenBrace) (groupOp TokCloseBrace) expr
      staticIdx = specialOp TokDot *> fmap (LitExpr . Fix . ExprLit . Sym) litUnquotedSymbol
  in  (,) <$> expr <*> (dynamicIdx <|> staticIdx)  <?> "index operator"

exprDebugger :: Parser ()
exprDebugger =
  symEq "debugger" $> () <?> "a debugger"

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
      body = stmtBody <|> exprBody
      exprBody = return <$> stmtWithPos (StmtExpr <$> expr)
      stmtBody = between (groupOp TokOpenBracket) (groupOp TokCloseBracket) (many stmt)
  in  (,) <$> args <*> (specialOp TokArrow *> body)

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

satisfyMaybe :: (Show a, Stream s Identity (SourcePos, a))
             => (a -> Maybe b)
             -> ParsecT s u Identity b
satisfyMaybe f =
  token (show . snd) fst (f . snd)

reservedWords :: [Text]
reservedWords =
  [ "def"
  , "debugger"
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

instance Functor ParseStmtF where
  fmap f (ParseStmtF (pos, s)) =
    ParseStmtF (pos, fmap f s)

instance Functor ParseMap where
  fmap f =
    ParseMap . map (fmap f) . runParseMap

instance Show (Fix ParseStmtF) where
  show =
    let phi pos (StmtExpr e) = ["StmtExpr", parens (show pos), show e]
        phi pos (StmtAssign s e) = ["StmtAssign", parens (show pos), show s, show e]
        phi pos (StmtIf cond andThen orElse) =
          ["StmtIf"
          , parens (show pos)
          , show cond
          , joinSpace andThen
          , joinSpace orElse
          ]
    in  cata (parens . joinSpace . uncurry phi . runParseStmtF)

instance Show (LitExpr (Expr Text TokBinOp) ParseLit) where
  show =
    let phi (ExprLit lit) = ["ExprLit", show lit]
        phi (ExprVar var) = ["ExprVar", show var]
        phi (ExprFuncall f args) = "ExprFuncall" : parens f : args
        phi (ExprIndex xs idx) = ["ExprIndex", xs, idx]
        phi (ExprTernary cond andThen orElse) = ["ExprTernary", cond, andThen, orElse]
        phi (ExprParen e) = ["ExprParen", e]
        phi (ExprNot e) = ["ExprNot", e]
        phi (ExprBinOp e o e') = ["ExprBinOp", e, show o, e']
        phi ExprDebugger = ["ExprDebugger"]
    in  cata (parens . joinSpace . phi) . runLitExpr

parens :: String -> String
parens x = '(' : x ++ ")"

joinSpace :: [String] -> String
joinSpace = intercalate " "
