module Imp.Parse (parseString, parseFile)  where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Imp.AST

languageDef =
  emptyDef { Token.commentStart    = "{-"
           , Token.commentEnd      = "-}"
           , Token.commentLine     = "--"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "input"
                                     , "output"
                                     , "to"
                                     , "from"
                                     ]
           , Token.reservedOpNames = [ "+", "-", ":=", "?", "="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parser :: Parser Cmd
parser = do
  whiteSpace
  c <- command
  eof
  return c

command :: Parser Cmd
command = parens command <|> sequenceOfCommand

sequenceOfCommand =
  do list <- (sepBy1 command' semi)
     return $ if length list == 1 then head list else Seq list

command' :: Parser Cmd
command' = ifCmd <|> skipCmd <|> randomCmd <|> assignCmd <|> inputCmd <|> outputCmd

ifCmd :: Parser Cmd
ifCmd =
  do reserved "if"
     cond <- bExpr
     reserved "then"
     cmd1 <- command
     reserved "else"
     cmd2 <- command
     return $ If cond cmd1 cmd2

assignCmd :: Parser Cmd
assignCmd =
  do var <- identifier
     reservedOp ":="
     expr <- aExpr
     return $ Assign var expr

randomCmd :: Parser Cmd
randomCmd =
  do var <- identifier
     reservedOp ":="
     expr1 <- aExpr
     reservedOp "?"
     expr2 <- aExpr
     return $ Random var expr1 expr2

inputCmd :: Parser Cmd
inputCmd =
  do reserved "input"
     var <- identifier
     reserved "from"
     agent <- identifier
     return $ Input var agent


outputCmd :: Parser Cmd
outputCmd =
  do reserved "output"
     expr <- aExpr
     reserved "to"
     agent <- identifier
     return $ Output expr agent

skipCmd :: Parser Cmd
skipCmd = reserved "skip" >> return Skip

aExpr :: Parser AExpr
aExpr = buildExpressionParser aOperators aTerm
  where aOperators = [ [Prefix (reservedOp "-" >> return (Neg             ))          ]
                     , [Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft]
                     , [Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]
                     ]
        aTerm = parens aExpr <|> liftM Var identifier <|> liftM IntConst integer

bExpr :: Parser BExpr
bExpr = buildExpressionParser bOperators bTerm
  where bOperators = [ [Prefix (reservedOp "not" >> return (Not        ))          ]
                     , [Infix  (reservedOp "and" >> return (BBinary And)) AssocLeft]
                     , [Infix  (reservedOp "or"  >> return (BBinary Or )) AssocLeft]
                     ]
        bTerm = parens bExpr
                <|> (reserved "true"  >> return (BoolConst True ))
                <|> (reserved "false" >> return (BoolConst False))
                <|> rExpr

rExpr =
  do a1 <- aExpr
     op <- relation
     a2 <- aExpr
     return $ RBinary op a1 a2
  where relation = (reservedOp ">" >> return Greater)
                   <|> (reservedOp "<" >> return Less)
                   <|> (reservedOp "=" >> return Equal)

parseString :: String -> Cmd
parseString str =
  case parse parser "" str of
   Left e -> error $ show e
   Right r -> r

parseFile :: String -> IO Cmd
parseFile file =
  do program <- readFile file
     return $ parseString program
