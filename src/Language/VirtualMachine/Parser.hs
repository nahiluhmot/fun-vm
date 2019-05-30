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
type Parser a = forall s. ParseStream s => ParsecT s () Identity a

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
  P.runParser (skip *> topLevel <* eof) ()

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
             <*> (symEq "import" *> skip *> rawUnreservedSymbol <* skip <* symEq "from")
             <*> (skip *> rawString <* stmtEnd)
  in  TopLevelImport <$> triple <?> "import"

topLevelDef :: Parser (TopLevel imp (SourcePos, Text, ParseExpr) stmt)
topLevelDef =
  let triple =
        tup3 <$> getPosition
             <*> (symEq "def" *> skip *> rawUnreservedSymbol)
             <*> (skip *> expr <* stmtEnd)
  in  TopLevelDef <$> triple <?> "def"

stmtEnd :: Parser ()
stmtEnd =
  (semicolon *> skip) <|> skipMany1 newline

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
  let cond = symEq "if" *> skip *> expr <* skip
      andThen = groupOp TokOpenCurly *> skip *> many (stmt <* skip) <* groupOp TokCloseCurly <* skip
      orElse = symEq "else" *> skip *> (andThen <|> fmap return (stmtWithPos stmtIf))
  in  StmtIf <$> cond <*> andThen <*> (try orElse <|> pure []) <?> "if statement"

stmtAssign :: Parser (Stmt Text ParseExpr stmt)
stmtAssign =
  StmtAssign <$> (symEq "let" *> skip *> rawUnreservedSymbol <* skip <* specialOp TokAssign)
             <*> (skip *> expr <* stmtEnd)
             <?> "assignment"

stmtExpr :: Parser (Stmt sym ParseExpr stmt)
stmtExpr =
  StmtExpr <$> expr <* stmtEnd

expr :: Parser ParseExpr
expr =
  let wrap = LitExpr . Fix . fmap runLitExpr

      simpleExpr :: Parser (Expr Text TokBinOp (ParseLit ParseExpr) ParseExpr)
      simpleExpr = try exprLit
               <|> exprVar
               <|> exprParen
               <|> exprNot
               <|> exprDebugger

      tryParseMore :: ParseExpr -> Parser ParseExpr
      tryParseMore ex =
        foldr (\func parser -> (func ex >>= tryParseMore . wrap) <|> parser)
              (pure ex)
              [ exprFuncall
              , exprIndex
              , exprBinOp
              , exprTernary
              ]
  in  simpleExpr >>= tryParseMore . wrap

exprVar :: Parser (Expr Text op lit expr)
exprVar =
  ExprVar <$> rawUnreservedSymbol <* skip <?> "variable"

exprParen :: Parser (Expr sym op lit ParseExpr)
exprParen =
  groupOp TokOpenParen *> skip *> (ExprParen <$> expr) <* skip <* groupOp TokCloseParen <* skip <?> "parenthetical expression"

exprNot :: Parser (Expr sym op lit ParseExpr)
exprNot =
  specialOp TokNot *> skip *> (ExprNot <$> expr)<* skip <?> "not operator"

exprDebugger :: Parser (Expr sym op lit expr)
exprDebugger =
  symEq "debugger" $> ExprDebugger <* skip <?> "debugger"

exprLit :: Parser (Expr sym op (ParseLit ParseExpr) expr)
exprLit =
  ExprLit <$> lit

exprFuncall :: ParseExpr -> Parser (Expr sym op lit ParseExpr)
exprFuncall func =
  ExprFuncall func <$> (skip *> list TokOpenParen TokCloseParen TokComma (expr <* skip) <* skip)
                   <?> "function call"

exprIndex :: ParseExpr -> Parser (Expr sym TokBinOp lit ParseExpr)
exprIndex ele =
  let staticIdx = specialOp TokDot *> skip *> fmap (LitExpr . Fix . ExprLit . Sym) rawUnquotedSymbol
      dynamicIdx = groupOp TokOpenSquare *> skip *> expr <* groupOp TokCloseSquare <* skip
  in  ExprIndex ele <$> (staticIdx <|> dynamicIdx) <* skip <?> "index operator"

exprBinOp :: ParseExpr -> Parser (Expr sym TokBinOp lit ParseExpr)
exprBinOp left =
  ExprBinOp left <$> (skip *> binOpAny <* skip) <*> expr <?> "binary operator"

exprTernary :: ParseExpr -> Parser (Expr sym op lit ParseExpr)
exprTernary cond =
  ExprTernary cond <$> (specialOp TokQuestionMark *> skip *> expr)
                   <*> (specialOp TokColon *> skip *> expr)
                   <?> "ternary condition"

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
  in  Func <$> ((,) <$> args <*> (skip *> specialOp TokThinArrow *> skip *> body <* skip)) <?> "function"

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
  specialOp TokColon *> (Sym <$> (rawUnquotedSymbol <|> rawString)) <* skip <?> "quoted symbol"

litNum :: Parser (Value sym int Rational str ParseFunction vec intMap ref)
litNum =
  let test (TokLit (TokNum n)) = Just (Float n)
      test _ = Nothing
  in  satisfyMaybe test <* skip <?> "number"

litString :: Parser (Value sym int float Text func vec intMap ref)
litString =
  Str <$> rawString <* skip <?> "string literal"

litNil :: Parser (Value sym int float str func vec intMap ref)
litNil =
  symEq "nil" $> Nil <* skip <?> "nil"

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

list :: TokGroupOp
     -> TokGroupOp
     -> TokSpecialOp
     -> Parser a
     -> Parser [a]
list begin end sep ele =
  tok (TokGroupOp begin) *> skip *> sepBy (ele <* skip) (tok (TokSpecialOp sep) <* skip) <* tok (TokGroupOp end) <* skip

semicolon :: Parser ()
semicolon =
  specialOp TokSemiColon $> ()

-- short name because it's everywhere
skip :: Parser ()
skip =
  many newline $> ()

newline :: Parser ()
newline =
  specialOp TokNewline $> ()

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

tok :: LexToken -> Parser LexToken
tok o =
  tokSatisfy (== o) <?> "token: " ++ show o

tokSatisfy :: (LexToken -> Bool)  -> Parser LexToken
tokSatisfy f =
  let test o
        | f o = Just o
        | otherwise = Nothing
  in  satisfyMaybe test

satisfyMaybe :: (LexToken -> Maybe a) -> Parser a
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

parens :: String -> String
parens x = '(' : x ++ ")"

joinSpace :: [String] -> String
joinSpace = intercalate " "

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
