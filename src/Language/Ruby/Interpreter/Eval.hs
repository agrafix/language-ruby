{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Ruby.Interpreter.Eval
    ( runProgram )
where

import Language.Ruby.Parser.AST

import Control.Monad.Except
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T

class MonadRuntime m where
    printLine :: T.Text -> m ()

instance MonadIO m => MonadRuntime (ExceptT e m) where
    printLine = liftIO . T.putStrLn

type MonadEval m = (MonadRuntime m, MonadError T.Text m)

runProgram :: Program -> IO (Either T.Text Literal)
runProgram p =
    runExceptT $
    do r <- mapM evalExpr (F.toList $ p_body p)
       case reverse r of
         [] -> pure LVoid
         (x : _) -> pure x

runtime :: MonadEval m => HM.HashMap Ident ([Literal] -> m Literal)
runtime =
    HM.fromList
    [ ( Ident "puts", \args -> printLine (T.pack $ show args) >> pure LVoid)
    ]

evalExpr :: MonadEval m => Expr -> m Literal
evalExpr e =
    case e of
      ELit l -> pure l
      EBinOp bo -> evalBinOp bo
      EFunCall fc -> evalFunCall fc
      _ -> throwError (T.pack $ "Not implemented: " ++ show e)

evalFunCall :: MonadEval m => FunCall -> m Literal
evalFunCall fc =
    mapM evalExpr (fc_args fc) >>= \args ->
    case HM.lookup (fc_ident fc) runtime of
      Just f ->
          f args
      Nothing ->
          throwError (T.pack $ "Only built in functions supported, not " ++ show (fc_ident fc))

evalBinOp :: forall m. MonadEval m => BinOp -> m Literal
evalBinOp bo =
    case bo of
      BoAdd e1 e2 -> runNumOp (+) e1 e2
      BoSub e1 e2 -> runNumOp (-) e1 e2
      BoMul e1 e2 -> runNumOp (*) e1 e2
      _ -> throwError (T.pack $ "Not implemented: " ++ show bo)
    where
      runNumOp ::
          (forall x. Num x => x -> x -> x) -> Expr -> Expr -> m Literal
      runNumOp op e1 e2 =
          do l1 <- evalExpr e1
             l2 <- evalExpr e2
             case (l1, l2) of
               (LInt i1, LInt i2) -> pure $ LInt $ i1 `op` i2
               (LDouble i1, LDouble i2) -> pure $ LDouble $ i1 `op` i2
               (LInt i1, LDouble i2) -> pure $ LDouble $ fromIntegral i1 `op` i2
               (LDouble i1, LInt i2) -> pure $ LDouble $ i1 `op` fromIntegral i2
               _ -> throwError "Oh shit."
