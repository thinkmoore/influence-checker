module Imp.Pretty (prettyPrint) where

import Imp.AST

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Data.List
import System.IO

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Builder as Builder

type Output a = ReaderT BS.ByteString (Writer Builder) a

runOutput :: BS.ByteString -> Output () -> Builder
runOutput indent o = execWriter $ runReaderT o indent

prettyPrint :: Cmd -> IO ()
prettyPrint cmd = Builder.hPutBuilder stdout $ runOutput BS.empty $ nlAfter $ printCmd cmd

printStr :: String -> Output ()
printStr = tell . Builder.lazyByteString . BS.pack

nlAfter :: Output () -> Output ()
nlAfter o = o >> printStr "\n"

printIndent :: Output ()
printIndent = do
  indent <- ask
  tell . Builder.lazyByteString $ indent

printCmd :: Cmd -> Output ()
printCmd (Seq cmds) = sequence_ . intersperse (printStr ";\n") $ map printCmd cmds
printCmd (Assign var expr) = do
  printIndent
  printStr var
  printStr " := "
  printAExpr expr
printCmd (Random var expr1 expr2) = do
  printIndent
  printStr var
  printStr " := "
  printAExpr expr1
  printStr " ? "
  printAExpr expr2
printCmd (Input var agent) = do
  printIndent
  printStr "input "
  printStr var
  printStr " from "
  printStr agent
printCmd (Output expr agent) = do
  printIndent
  printStr "output "
  printAExpr expr
  printStr " to "
  printStr agent
printCmd (Skip) = do
  printIndent
  printStr "skip"
printCmd (If cond cmd1 cmd2) = do
  printIndent
  printStr "if "
  printBExpr cond
  printStr " then (\n"
  nlAfter $ local (BS.append $ BS.pack "  ") $ printCmd cmd1
  printIndent
  printStr ") else (\n"
  nlAfter $ local (BS.append $ BS.pack "  ") $ printCmd cmd2
  printIndent
  printStr ")"

inParens :: Output () -> Output ()
inParens o = do
  printStr "("
  o
  printStr ")"

printAExpr (Var v) = do
  printStr v
printAExpr (IntConst i) = do
  printStr $ show i
printAExpr (Neg expr) = do
  printStr "- "
  printAExprParens expr
printAExpr (ABinary Add expr1 expr2) = do
  printAExprParens expr1
  printStr " + "
  printAExprParens expr2
printAExpr (ABinary Subtract expr1 expr2) = do
  printAExprParens expr1
  printStr " - "
  printAExprParens expr2

printAExprParens (ABinary op e1 e2) =
  inParens $ printAExpr (ABinary op e1 e2)
printAExprParens (Neg e) =
  inParens $ printAExpr (Neg e)
printAExprParens ae =
  printAExpr ae

printBExpr (BoolConst b) = do
  printStr $ show b
printBExpr (Not expr) = do
  printStr "not "
  printBExprParens expr
printBExpr (BBinary And expr1 expr2) = do
  printBExprParens expr1
  printStr " and "
  printBExprParens expr2
printBExpr (BBinary Or expr1 expr2) = do
  printBExprParens expr1
  printStr " or "
  printBExprParens expr2
printBExpr (RBinary Greater expr1 expr2) = do
  printAExprParens expr1
  printStr " > "
  printAExprParens expr2
printBExpr (RBinary Less expr1 expr2) = do
  printAExprParens expr1
  printStr " < "
  printAExprParens expr2
printBExpr (RBinary Equal expr1 expr2) = do
  printAExprParens expr1
  printStr " = "
  printAExprParens expr2

printBExprParens (BoolConst b) =
  printBExpr (BoolConst b)
printBExprParens be =
  inParens $ printBExpr be
