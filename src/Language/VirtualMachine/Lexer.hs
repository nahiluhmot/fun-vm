{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Language.VirtualMachine.Lexer ( runLexer
                                     , LexStream
                                     , LexToken
                                     , Tok(..)
                                     , TokGroupOp(..)
                                     , TokBinOp(..)
                                     , TokSpecialOp(..)
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
type LexToken = Tok TokGroupOp TokBinOp TokSpecialOp (TokLit Text Rational Text)

runLexer :: LexStream s => SourceName -> s -> Either ParseError [(SourcePos, LexToken)]
runLexer =
  runParser (toks <* eof) ()

toks :: Lexer [(SourcePos, LexToken)]
toks =
  whitespace *> sepEndBy ((,) <$> getPosition <*> tok) whitespace

whitespace :: Lexer ()
whitespace =
  let commentLine = char '#' *> manyTill anyChar (char '\n') *> spaces
  in  spaces *> optional (many commentLine)

tok :: Lexer LexToken
tok =
  let lit = TokSym . pack <$> tokSym
        <|> TokStr . pack <$> tokStr
        <|> TokNum <$> try tokNum
  in  TokLit <$> lit <|> tokStringOp <|> tokCharOp

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
      <*> many (alphaNum <|> char '_' <|> char '?')

tokStr :: Lexer String
tokStr =
  let parseUnescaped =
        (++) <$> many (noneOf "\"\\")
             <*> ((char '"' $> "") <|> ((++) <$> parseEscaped <*> parseUnescaped))
      parseEscaped =
        char '\\' *>
        (satisfyMaybe (flip lookup charToEscapeCode) <|> fmap (\c -> '\\' : c : []) anyChar)
  in  char '"' *> parseUnescaped <?> "string"

tokStringOp :: Lexer LexToken
tokStringOp =
  let tryOp op t acc =
        try ((string op $> t) <?> "operator: " ++ op) <|> acc
  in  foldrWithKey tryOp parserZero stringToOp

tokCharOp :: Lexer LexToken
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

stringToOp :: Map String LexToken
stringToOp =
  fromList [ ("==", TokBinOp TokEq)
           , ("!=", TokBinOp TokNeq)
           , (">=", TokBinOp TokGte)
           , ("<=", TokBinOp TokLte)
           , ("&&", TokBinOp TokAnd)
           , ("||", TokBinOp TokOr)
           , ("**", TokBinOp TokPow)
           , ("->", TokSpecialOp TokThinArrow)
           , ("=>", TokSpecialOp TokFatArrow)
           , ("...", TokSpecialOp TokSpread)
           , ("..", TokSpecialOp TokRange)
           ]

charToOp :: Map Char LexToken
charToOp =
  fromList $ [ ('(', TokGroupOp TokOpenParen)
             , (')', TokGroupOp TokCloseParen)
             , ('[', TokGroupOp TokOpenSquare)
             , (']', TokGroupOp TokCloseSquare)
             , ('{', TokGroupOp TokOpenCurly)
             , ('}', TokGroupOp TokCloseCurly)
             , ('+', TokBinOp TokPlus)
             , ('-', TokBinOp TokMinus)
             , ('/', TokBinOp TokDiv)
             , ('*', TokBinOp TokMul)
             , ('<', TokBinOp TokLt)
             , ('>', TokBinOp TokGt)
             , (',', TokSpecialOp TokComma)
             , ('.', TokSpecialOp TokDot)
             , ('!', TokSpecialOp TokNot)
             , ('=', TokSpecialOp TokAssign)
             , (':', TokSpecialOp TokColon)
             , (';', TokSpecialOp TokSemiColon)
             , ('?', TokSpecialOp TokQuestionMark)
             ]

data Tok group binary special lit
  = TokLit lit
  | TokGroupOp group
  | TokSpecialOp special
  | TokBinOp binary
  deriving (Eq, Show)

data TokGroupOp
  = TokOpenParen
  | TokCloseParen
  | TokOpenSquare
  | TokCloseSquare
  | TokOpenCurly
  | TokCloseCurly
  deriving (Eq, Ord, Enum, Show)

-- Sorted in ascending order of precedence
data TokBinOp
  = TokPlus
  | TokMinus
  | TokDiv
  | TokMul
  | TokPow
  | TokAnd
  | TokOr
  | TokEq
  | TokNeq
  | TokGt
  | TokGte
  | TokLt
  | TokLte
  deriving (Eq, Ord, Enum, Bounded, Show)

data TokSpecialOp
  = TokComma
  | TokDot
  | TokNot
  | TokColon
  | TokAssign
  | TokThinArrow
  | TokFatArrow
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
