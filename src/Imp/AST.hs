module Imp.AST (BExpr(..), BBinOp(..), RBinOp(..), AExpr(..), ABinOp(..), Cmd(..)) where

{-
     a ::= x | n | - a | a opa a
     b ::= true | false | not b | b opb b | a opr a
   opa ::= + | -
   opb ::= and | or
   opr ::= > | < | =
     c ::= x := a | skip | c ; c | ( c ) | if b then c else c
         | input x from Agent | output x to Agent | x := a ? a
-}

data BExpr = BoolConst Bool
     	   | Not BExpr
	   | BBinary BBinOp BExpr BExpr
	   | RBinary RBinOp AExpr AExpr
	   deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less | Equal deriving (Show)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = Add | Subtract deriving (Show)

data Cmd = Seq [Cmd]
         | Assign String AExpr
         | Random String AExpr AExpr
         | If BExpr Cmd Cmd
         | Input String String
         | Output AExpr String
         | Skip
         deriving (Show)
