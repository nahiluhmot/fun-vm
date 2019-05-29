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

import Data.List (intercalate)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)

import qualified Data.Set as S
import Data.Text (Text)
import Text.Parsec (ParseError, SourceName, SourcePos)
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (runParser)
import qualified Text.Parsec.Prim as P

import Language.VirtualMachine.Data.AST (TopLevel(..), Stmt(..), Expr(..), LitExpr(..))
import Language.VirtualMachine.Data.Fix (Fix(..), cata)
import Language.VirtualMachine.Data.Value (Value(..))
import Language.VirtualMachine.Lexer (LexToken, Tok(..), TokLit(..), TokGroupOp(..), TokBinOp(..), TokSpecialOp(..))

type ParseInput = (SourcePos, LexToken)
type ParseStream s = Stream s Identity ParseInput
type Parser a = forall s. ParseStream s => ParsecT s ParserState Identity a
-- For detecting recursive expressions
type ParserState = S.Set (RecExpr, SourcePos)
data RecExpr
  = RecFuncall
  | RecIndex
  | RecTernary
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
  (++) <$> many topLevelImport
       <*> many (topLevelDef <|> topLevelStmt)

topLevelStmt :: Parser (TopLevel imp def ParseStmt)
topLevelStmt =
  TopLevelStmt <$> stmt

topLevelImport :: Parser (TopLevel (SourcePos, Text, Text) def stmt)
topLevelImport =
  let triple =
        tup3 <$> getPosition
             <*> (symEq "import" *> rawUnreservedSymbol <* symEq "from")
             <*> (rawString <* stmtEnd)
  in  TopLevelImport <$> triple <?> "import"

topLevelDef :: Parser (TopLevel imp (SourcePos, Text, ParseExpr) stmt)
topLevelDef =
  let triple =
        tup3 <$> getPosition
             <*> (symEq "def" *> rawUnreservedSymbol)
             <*> (expr <* stmtEnd)
  in  TopLevelDef <$> triple <?> "def"

stmt :: Parser ParseStmt
stmt =
  let chooseStmt (TokLit (TokSym "let")) = stmtAssign
      chooseStmt (TokLit (TokSym "if")) = stmtIf
      chooseStmt _ = stmtExpr
  in  stmtWithPos $ lookAhead anyToken >>= chooseStmt . snd

stmtWithPos :: Parser (Stmt Text ParseExpr ParseStmt) -> Parser ParseStmt
stmtWithPos action =
  let wrap pos a = Fix $ ParseStmtF (pos, a)
  in  wrap <$> getPosition <*> action

stmtIf :: Parser (Stmt sym ParseExpr ParseStmt)
stmtIf =
  let cond = symEq "if" *> expr
      andThen = groupOp TokOpenCurly *> many stmt <* groupOp TokCloseCurly
      orElse = symEq "else" *> (andThen <|> fmap return (stmtWithPos stmtIf))
  in  StmtIf <$> cond <*> andThen <*> (try orElse <|> pure []) <?> "if statement"

stmtAssign :: Parser (Stmt Text ParseExpr stmt)
stmtAssign =
  StmtAssign <$> (symEq "let" *> rawUnreservedSymbol <* specialOp TokAssign)
             <*> (expr <* stmtEnd)
             <?> "assignment"

stmtExpr :: Parser (Stmt sym ParseExpr stmt)
stmtExpr =
  StmtExpr <$> expr <* stmtEnd

stmtEnd :: Parser ()
stmtEnd = optional $ specialOp TokSemiColon

expr :: Parser ParseExpr
expr =
  let parseExpr = recLock RecFuncall (try exprFuncall)
              <|> recLock RecIndex (try exprIndex)
              <|> recLock RecBinOp (try exprBinOp)
              <|> recLock RecTernary (try exprTernary)
              <|> try exprLit
              <|> exprVar
              <|> exprParen
              <|> exprNot
              <|> exprDebugger
  in  LitExpr . Fix . fmap runLitExpr <$> parseExpr

recLock :: RecExpr -> Parser a -> Parser a
recLock recExpr child =
  let withLock record forbidden
        | S.member record forbidden =
          parserZero
        | otherwise =
          putState (S.insert record forbidden) *> child <* putState forbidden
  in  getParserState >>= \(State _ pos forbidden) -> withLock (recExpr, pos) forbidden

exprVar :: Parser (Expr Text op lit expr)
exprVar =
  ExprVar <$> rawUnreservedSymbol <?> "variable"

exprParen :: Parser (Expr sym op lit ParseExpr)
exprParen =
  groupOp TokOpenParen *> (ExprParen <$> expr) <* groupOp TokCloseParen <?> "parenthetical expression"

exprBinOp :: Parser (Expr sym TokBinOp lit ParseExpr)
exprBinOp =
  ExprBinOp <$> expr <*> binOpAny <*> expr <?> "binary operator"

exprNot :: Parser (Expr sym op lit ParseExpr)
exprNot =
  specialOp TokNot *> (ExprNot <$> expr) <?> "not operator"

exprTernary :: Parser (Expr sym op lit ParseExpr)
exprTernary =
  ExprTernary <$> (expr <* specialOp TokQuestionMark)
              <*> expr
              <*> (specialOp TokColon *> expr)
              <?> "ternary operator"

exprIndex :: Parser (Expr sym op lit ParseExpr)
exprIndex =
  let staticIdx = specialOp TokDot *> fmap (LitExpr . Fix . ExprLit . Sym) rawUnquotedSymbol
      dynamicIdx = groupOp TokOpenSquare *> expr <* groupOp TokCloseSquare
  in  ExprIndex <$> expr <*> (staticIdx <|> dynamicIdx) <?> "index operator"

exprDebugger :: Parser (Expr sym op lit expr)
exprDebugger =
  symEq "debugger" $> ExprDebugger <?> "debugger"

exprFuncall :: Parser (Expr sym op lit ParseExpr)
exprFuncall =
  ExprFuncall <$> expr
              <*> list TokOpenParen TokCloseParen TokComma expr
              <?> "function call"

exprLit :: Parser (Expr sym op (ParseLit ParseExpr) expr)
exprLit =
  ExprLit <$> lit

lit :: Parser (ParseLit ParseExpr)
lit =
  let chooseLit (TokLit (TokSym "nil")) = litNil
      chooseLit (TokSpecialOp TokColon) = litQuotedSymbol
      chooseLit (TokLit (TokNum _)) = litNum
      chooseLit (TokLit (TokStr _)) = litString
      chooseLit (TokGroupOp TokOpenSquare) = litVec
      chooseLit (TokGroupOp TokOpenCurly) = litMap
      chooseLit (TokGroupOp TokOpenParen) = litFunction
      chooseLit (TokLit (TokSym _)) = litFunction
      chooseLit _ = parserZero
  in  lookAhead anyToken >>= chooseLit . snd

litFunction :: Parser (Value sym int float str ParseFunction vec intMap ref)
litFunction =
  let args = singleArg <|> multipleArgs
      singleArg = fmap (\x -> [x]) rawUnreservedSymbol
      multipleArgs = list TokOpenParen TokCloseParen TokComma rawUnreservedSymbol
      body = stmtBody <|> exprBody
      exprBody = return <$> stmtWithPos (StmtExpr <$> expr)
      stmtBody = between (groupOp TokOpenCurly) (groupOp TokCloseCurly) (many stmt)
  in  Func <$> ((,) <$> args <*> (specialOp TokThinArrow *> body)) <?> "function"

litMap :: Parser (Value sym int float str func vec ParseMap ParseExpr)
litMap =
  let key :: Parser ParseExpr
      key = try (LitExpr . Fix . ExprLit . Sym <$> keyLit) <|> keyExpr
      keyLit :: Parser Text
      keyLit = (rawUnquotedSymbol <|> rawString) <* specialOp TokColon
      keyExpr :: Parser ParseExpr
      keyExpr = expr <* specialOp TokFatArrow
      entries = list TokOpenCurly TokCloseCurly TokComma ((,) <$> key <*> expr) <?> "map literal"
  in  Map . ParseMap <$> entries

litVec :: Parser (Value sym int float str func [] intMap ParseExpr)
litVec =
  Vector <$> list TokOpenSquare TokCloseSquare TokComma expr <?> "vector literal"

litQuotedSymbol :: Parser (Value Text int float str func vec intMap ref)
litQuotedSymbol =
  specialOp TokColon *> (Sym <$> (rawUnquotedSymbol <|> rawString)) <?> "quoted symbol"

litNum :: Parser (Value sym int Rational str ParseFunction vec intMap ref)
litNum =
  let test (TokLit (TokNum n)) = Just (Float n)
      test _ = Nothing
  in  satisfyMaybe test <?> "number"

litString :: Parser (Value sym int float Text func vec intMap ref)
litString =
  Str <$> rawString <?> "string literal"

litNil :: Parser (Value sym int float str func vec intMap ref)
litNil =
  symEq "nil" $> Nil

rawUnreservedSymbol :: Parser Text
rawUnreservedSymbol =
  let test (TokLit (TokSym s))
        | elem s reservedWords = Nothing
        | otherwise = Just s
      test _ = Nothing
  in  satisfyMaybe test <?> "unreserved symbol"

rawUnquotedSymbol :: Parser Text
rawUnquotedSymbol =
  let test (TokLit (TokSym str)) = Just str
      test _ = Nothing
  in  satisfyMaybe test <?> "unquoted symbol"

rawString :: Parser Text
rawString =
  let test (TokLit (TokStr str)) = Just str
      test _ = Nothing
  in  satisfyMaybe test <?> "string literal"

symEq :: Text -> Parser Text
symEq s =
  let test (TokLit (TokSym s'))
        | s == s' = Just s
        | otherwise = Nothing
      test _ = Nothing
  in  satisfyMaybe test <?> "symbol: " ++ show s

groupOp :: TokGroupOp -> Parser TokGroupOp
groupOp o =
  let test (TokGroupOp o')
        | o == o' = Just o
        | otherwise = Nothing
      test _ = Nothing
  in  satisfyMaybe test <?> "group operator: " ++ show o

specialOp :: TokSpecialOp -> Parser TokSpecialOp
specialOp o =
  let test (TokSpecialOp o')
        | o == o' = Just o
        | otherwise = Nothing
      test _ = Nothing
  in  satisfyMaybe test <?> "special operator: " ++ show o

binOpAny :: Parser TokBinOp
binOpAny =
  let test (TokBinOp o) = Just o
      test _ = Nothing
  in  satisfyMaybe test <?> "binary operator"

list :: TokGroupOp
     -> TokGroupOp
     -> TokSpecialOp
     -> Parser a
     -> Parser [a]
list begin end sep ele =
  tok (TokGroupOp begin) *> sepBy ele (tok (TokSpecialOp sep)) <* tok (TokGroupOp end)

tok :: LexToken -> Parser LexToken
tok o =
  tokSatisfy (== o) <?> "operator: " ++ show o

tokSatisfy :: (LexToken -> Bool)  -> Parser LexToken
tokSatisfy f =
  let test o
        | f o = Just o
        | otherwise = Nothing
  in  satisfyMaybe test

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
    let phi (ExprLit l) = ["ExprLit", show l]
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
