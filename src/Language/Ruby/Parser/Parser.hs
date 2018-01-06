{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Ruby.Parser.Parser
    ( parseProgram
    , execParser
    )
where

import Language.Ruby.Parser.AST
import qualified Data.HashMap.Strict as HM

import Control.Monad (void, when)
import Data.Bifunctor
import Data.Maybe
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
    Program <$> parseExprSeq <* eof

parseExprSeq :: Parser (Seq.Seq Expr)
parseExprSeq =
    Seq.fromList <$> many (parseExpr <* delim)
    where
      delim =
          void (some (symbol "\n"))
          <|> void semi
          <|> void (semi <* some (symbol "\n"))

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
    <|> (EFunDef <$> funDef)
    <|> (EIfThenElse <$> try ifThenElse)
    <|> (ELit <$> lit)
    <|> (EFunCall <$> funCall)
    <|> (EVar <$> identifier)
    <|> (EHash <$> rhash)
    <?> "term"

funDef :: Parser FunDef
funDef =
    do rword "def"
       fd_name <- identifier
       args <-
           optional $
           do void $ symbol "("
              x <- identifier `sepBy` symbol ","
              void $ symbol ")"
              pure x
       let fd_args = fromMaybe [] args
       void $ symbol "\n"
       fd_body <- parseExprSeq
       rword "end"
       pure FunDef {..}

funCall :: Parser FunCall
funCall =
    do fc_ident <- identifier
       openBrac <- optional (symbol "(")
       fc_args <- expr `sepBy` symbol ","
       when (isJust openBrac) $ void $ symbol ")"
       pure FunCall {..}

rsymbol :: Parser Symbol
rsymbol =
    symbol ":" *> (Symbol . unIdent <$> identifier)

rhash :: Parser (HM.HashMap HashKey Expr)
rhash =
    do openBrac <- optional (symbol "{")
       mp <- kvPair `sepBy` symbol ","
       when (isJust openBrac) $ void $ symbol "}"
       when (isNothing openBrac && null mp) $ fail "Empty hash w/o {} not allowed."
       pure $ HM.fromList mp
    where
        kvKey =
            HkSymbol <$> (Symbol . unIdent <$> identifier)
        kvPair =
            do k <- kvKey
               void $ symbol ":"
               val <- expr
               pure (k, val)

lit :: Parser Literal
lit =
    (LDouble <$> try double)
    <|> (LInt <$> integer)
    <|> (LBool True <$ symbol "true")
    <|> (LBool False <$ symbol "false")
    <|> (LSymbol <$> rsymbol)

table :: [[Operator Parser Expr]]
table =
    [ [ binary  "*"  (binOp2 BoMul)
      , binary  "/"  (binOp2 BoDiv)
      ]
    , [ binary  "+"  (binOp2 BoAdd)
      , binary  "-"  (binOp2 BoSub)
      ]
    ]

--binOp1 :: (Expr -> BinOp) -> Expr -> Expr
--binOp1 f = EBinOp . f

binOp2 :: (Expr -> Expr -> BinOp) -> Expr -> Expr -> Expr
binOp2 f a b = EBinOp $ f a b

binary :: T.Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)
--prefix :: T.Text -> (a -> a) -> Operator Parser a
--prefix  name f = Prefix  (f <$ symbol name)
--postfix :: T.Text -> (a -> a) -> Operator Parser a
--postfix name f = Postfix (f <$ symbol name)

ifThenElse :: Parser IfThenElse
ifThenElse =
    do rword "if"
       cond <- parseExpr
       rword "then"
       body <- parseExpr
       elsif <-
           many $
           do rword "elsif"
              condE <- parseExpr
              rword "then"
              bodyE <- parseExpr
              pure (condE, bodyE)
       els <-
           optional $
           do rword "else"
              parseExpr
       rword "end"
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
