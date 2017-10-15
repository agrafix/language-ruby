module Language.Ruby.Parser.AST where

import qualified Data.Sequence as Seq
import qualified Data.Text as T

data Program
    = Program
    { p_body :: !(Seq.Seq Expr)
    } deriving (Show, Eq)

data Literal
    = LString !T.Text
    | LInt !Int
    | LDouble !Double
    | LBool !Bool
    deriving (Show, Eq)

newtype Ident
    = Ident { unIdent :: T.Text }
    deriving (Show, Eq)

data Expr
    = ELit !Literal
    | EBinOp !BinOp
    | EVar !Ident
    | EIfThenElse !IfThenElse
    deriving (Show, Eq)

data IfThenElse
    = IfThenElse
    { ite_if :: !(Expr, Expr)
    , ite_elsif :: ![(Expr, Expr)]
    , ite_else :: !(Maybe Expr)
    } deriving (Show, Eq)

data BinOp
    = BoAdd !Expr !Expr
    | BoMul !Expr !Expr
    | BoSub !Expr !Expr
    | BoDiv !Expr !Expr
    | BoNegate !Expr
    deriving (Show, Eq)
