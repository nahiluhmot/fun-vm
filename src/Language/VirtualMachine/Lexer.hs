{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Language.VirtualMachine.Lexer ( runLexer
                                     , LexStream
                                     , LexToken
                                     , Tok(..)
                                     , TokOp(..)
                                     , TokLit(..)
                                     ) where

import Prelude hiding (lookup)

import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)

import Data.Map (Map, foldrWithKey, fromList, lookup)
import Data.Text (Text, pack)

import Text.Parsec
import Text.Parsec.Pos (updatePosChar)

type LexStream s = Stream s Identity Char
type Lexer a = forall s. LexStream s => ParsecT s () Identity a
type LexToken = Tok TokOp (TokLit Text Rational Text)

runLexer :: LexStream s => SourceName -> s -> Either ParseError [(SourcePos, LexToken)]
runLexer =
  runParser (toks <* eof) ()

toks :: Lexer [(SourcePos, LexToken)]
toks =
  spaces *> sepEndBy ((,) <$> getPosition <*> tok) spaces

tok :: Lexer LexToken
tok =
  let lit = TokSym . pack <$> tokSym
        <|> TokStr . pack <$> tokStr
        <|> TokNum <$> try tokNum
      op = tokStringOp
       <|> tokCharOp
  in  TokLit <$> lit <|> TokOp <$> op

tokNum :: Fractional num => Lexer num
tokNum =
  let sign = option id (char '-' $> negate)
      beforeDecimal = foldl decimalShiftUp 0 . map fromIntegral <$> digits
      afterDecimal = option 0 . try $ char '.' *> (foldr decimalShiftDown 0 . map fromIntegral <$> digits)
      decimalShiftUp acc ele = (acc * 10) + ele
      decimalShiftDown ele acc = (acc + ele) / 10
      num = sign <*> ((+) <$> beforeDecimal <*> afterDecimal) <?> "number"
  in  num

tokSym :: Lexer String
tokSym =
  (:) <$> (letter <|> char '_')
      <*> many (alphaNum <|> char '_')

tokStr :: Lexer String
tokStr =
  let parseUnescaped =
        (++) <$> many (noneOf "\"\\")
             <*> ((char '"' $> "") <|> ((++) <$> parseEscaped <*> parseUnescaped))
      parseEscaped =
        char '\\' *>
        (satisfyMaybe (flip lookup charToEscapeCode) <|> fmap (\c -> '\\' : c : []) anyChar)
  in  char '"' *> parseUnescaped <?> "string"

tokStringOp :: Lexer TokOp
tokStringOp =
  let tryOp op t acc =
        try ((string op $> t) <?> "operator: " ++ op) <|> acc
  in  foldrWithKey tryOp parserZero stringToOp

tokCharOp :: Lexer TokOp
tokCharOp =
  satisfyMaybe (flip lookup charToOp) <?> "single character operator"

digits :: Lexer [Int]
digits =
  map digitToInt <$> many1 digit

satisfyMaybe :: (Char -> Maybe a) -> Lexer a
satisfyMaybe =
  tokenPrim (\c -> show [c])
            (\pos c _cs -> updatePosChar pos c)

charToEscapeCode :: Map Char String
charToEscapeCode =
  fromList [ ('a', "\a")
           , ('b', "\b")
           , ('f', "\f")
           , ('n', "\n")
           , ('r', "\r")
           , ('t', "\t")
           , ('v', "\v")
           , ('\\', "\\")
           ]

stringToOp :: Map String TokOp
stringToOp =
  fromList [ ("==", TokEq)
           , ("!=", TokNeq)
           , (">=", TokGte)
           , ("<=", TokLte)
           , ("&&", TokAnd)
           , ("||", TokOr)
           , ("**", TokPow)
           , ("=>", TokArrow)
           , ("...", TokSpread)
           , ("..", TokRange)
           ]

charToOp :: Map Char TokOp
charToOp =
  fromList $ [ ('(', TokOpenParen)
             , (')', TokCloseParen)
             , ('[', TokOpenBrace)
             , (']', TokCloseBrace)
             , ('{', TokOpenBracket)
             , ('}', TokCloseBracket)
             , (',', TokComma)
             , ('.', TokDot)
             , ('!', TokNot)
             , ('+', TokPlus)
             , ('-', TokMinus)
             , ('/', TokDiv)
             , ('*', TokMul)
             , ('=', TokAssign)
             , ('<', TokLt)
             , ('>', TokGt)
             , (':', TokColon)
             , (';', TokSemiColon)
             , ('?', TokQuestionMark)
             ]

data Tok op lit
  = TokOp op
  | TokLit lit
  deriving (Eq, Show)

data TokOp
  = TokOpenParen
  | TokCloseParen
  | TokOpenBrace
  | TokCloseBrace
  | TokOpenBracket
  | TokCloseBracket
  | TokComma
  | TokDot
  | TokNot
  | TokPlus
  | TokMinus
  | TokDiv
  | TokMul
  | TokPow
  | TokAnd
  | TokOr
  | TokColon
  | TokEq
  | TokNeq
  | TokGt
  | TokGte
  | TokLt
  | TokLte
  | TokAssign
  | TokArrow
  | TokRange
  | TokSpread
  | TokSemiColon
  | TokQuestionMark
  deriving (Eq, Show)

data TokLit sym num str
  = TokSym sym
  | TokNum num
  | TokStr str
  deriving (Eq, Show)
