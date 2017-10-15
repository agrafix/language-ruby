{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Ruby.Parser.Parser
    ( parseProgram
    , execParser
    )
where

import Language.Ruby.Parser.AST

import Control.Monad (void)
import Data.Bifunctor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

execParser :: String -> Parser x -> T.Text -> Either T.Text x
execParser name p inp =
    first (T.pack . parseErrorPretty) $ runParser p name inp

-- | Ruby programs are sequence of expressions. Each expression are delimited by
-- semicolons(;) or newlines. Backslashes at the end of line does not terminate
-- expression.
parseProgram :: Parser Program
parseProgram =
    (Program . Seq.fromList) <$> many (parseExpr <* delim) <* eof
    where
      delim =
          symbol "\n"
          <|> semi
          <|> (semi <* symbol "\n")

parseExpr :: Parser Expr
parseExpr = lexeme expr

vspace1 :: (MonadParsec e s m, Token s ~ Char) => m ()
vspace1 = void $ takeWhile1P (Just "vertical white space") (\c -> c == ' ' || c == '\t')
{-# INLINE vspace1 #-}

sc :: Parser ()
sc = L.space vspace1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "=begin" "=end"

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term =
    parens expr
    <|> (EIfThenElse <$> try ifThenElse)
    <|> (ELit <$> lit)
    <|> (EVar <$> identifier)
    <?> "term"

lit :: Parser Literal
lit =
    (LDouble <$> try double)
    <|> (LInt <$> integer)
    <|> (LBool True <$ symbol "true")
    <|> (LBool False <$ symbol "false")

table :: [[Operator Parser Expr]]
table =
    [ [ prefix  "-"  (binOp1 BoNegate)
      , prefix  "+"  id
      ]
    , [ binary  "*"  (binOp2 BoMul)
      , binary  "/"  (binOp2 BoDiv)
      ]
    , [ binary  "+"  (binOp2 BoAdd)
      , binary  "-"  (binOp2 BoSub)
      ]
    ]

binOp1 :: (Expr -> BinOp) -> Expr -> Expr
binOp1 f = EBinOp . f

binOp2 :: (Expr -> Expr -> BinOp) -> Expr -> Expr -> Expr
binOp2 f a b = EBinOp $ f a b

binary :: T.Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)
prefix :: T.Text -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)
postfix :: T.Text -> (a -> a) -> Operator Parser a
postfix name f = Postfix (f <$ symbol name)

ifThenElse :: Parser IfThenElse
ifThenElse =
    do void $ symbol "if"
       cond <- parseExpr
       void $ symbol "then"
       body <- parseExpr
       elsif <-
           many $
           do void $ symbol "elsif"
              condE <- parseExpr
              void $ symbol "then"
              bodyE <- parseExpr
              pure (condE, bodyE)
       els <-
           optional $
           do void $ symbol "else"
              parseExpr
       void $ symbol "end"
       pure
           IfThenElse
           { ite_if = (cond, body)
           , ite_elsif = elsif
           , ite_else = els
           }

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

double :: Parser Double
double = lexeme L.float

semi :: Parser T.Text
semi = symbol ";"

rword :: T.Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [T.Text]
rws =
    [ "BEGIN", "END", "alias", "and", "begin", "break", "case"
    , "class", "def", "defined", "do", "else", "elsif", "end"
    , "ensure", "false", "for", "if", "in", "module", "next"
    , "nil", "not", "or", "redo", "rescue", "retry", "return"
    , "self", "super", "then", "true", "undef", "unless", "until"
    , "when", "while", "yield"
    ]

-- | Ruby identifiers are consist of alphabets, decimal digits, and the
-- underscore character, and begin with a alphabets(including underscore). There
-- are no restrictions on the lengths of Ruby identifiers.
identifier :: Parser Ident
identifier =
    Ident <$> (lexeme . try) (p >>= check)
    where
      p =
          (\x xs -> T.pack (x : xs))
          <$> (letterChar <|> char '_')
          <*> many (alphaNumChar <|> char '_')
      check x =
          if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x
