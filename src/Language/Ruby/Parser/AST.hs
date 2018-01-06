{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Ruby.Parser.AST where

import Data.Hashable
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T

data Program
    = Program
    { p_body :: !(Seq.Seq Expr)
    } deriving (Show, Eq)

newtype Symbol
    = Symbol { unSymbol :: T.Text }
    deriving (Show, Eq, Hashable)

data Literal
    = LString !T.Text
    | LSymbol !Symbol
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
    | EFunCall !FunCall
    | EHash !(HM.HashMap HashKey Expr)
    deriving (Show, Eq)

data HashKey
    = HkString !T.Text
    | HkSymbol !Symbol
    deriving (Show, Eq, Generic)

instance Hashable HashKey

data FunCall
    = FunCall
    { fc_ident :: !Ident
    , fc_args :: ![Expr]
    } deriving (Show, Eq)

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
