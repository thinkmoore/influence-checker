module Mck.AST where

newtype VarId = VarId String
newtype TypeId = TypeId String
newtype Agent = Agent String
newtype ProtocolId = ProtocolId String
newtype Label = Label String
newtype Constant = Const String

data JointProtocol = JointProtocol Environment [Protocol]

data Environment = Env [TypeDec] [VarDec] [Definition] (Maybe EnvInitCond) [EnvAgentDec] (Maybe TransitionBlock) [EnvFairness] [EnvSpec]

data TypeDec = TypeDec TypeId VarType

data Definition = Def Var Expr

data EnvAgentDec = EnvAgentDec Agent ProtocolId [Var]

data EnvInitCond = InitDec InitFromDec TransitionBlock

data EnvFairness = Fairness BoolTransitionExpr

data InitFromDec = Uniform | AllInit

data TransitionBlock = Block [TransitionStmt]

data TransitionStmt = TBlockStmt TransitionBlock
                    | TIfClause TransitionClause [TransitionClause] (Maybe TransitionStmt)
                    | TIfBool BoolTransitionExpr TransitionStmt TransitionStmt
                    | TSkip
                    | TAssign VarId Expr
                    | TRandom [Var] BoolExpr

data TransitionClause = TC BoolTransitionExpr TransitionStmt

data Protocol = Protocol ProtocolId EnvVarParams LocalVars [Definition] ProtocolBlock

data EnvVarParams = EV [VarDec]

data LocalVars = LV [VarDec] (Maybe LocalVarInitCond)

data LocalVarInitCond = LVAllInit | LVExpr Expr | LVInitDec InitFromDec TransitionBlock

data ProtocolBlock = PBlock [LabelledStmt]

data LabelledStmt = LS ProtocolStmt (Maybe Label)

data ProtocolStmt = PBlockStmt ProtocolBlock
                  | PIfClause Clause [Clause] (Maybe LabelledStmt)
                  | PDo Clause [Clause] (Maybe LabelledStmt) (Maybe LabelledStmt)
                  | PIfBool BoolExpr LabelledStmt LabelledStmt
                  | PWhile BoolExpr LabelledStmt
                  | PSkip
                  | PAssign VarId Expr
                  | PRandom [Var] BoolExpr
                  | PAction Action [ActionAssignment]

data Clause = Clause BoolExpr LabelledStmt

data Action = AConstant Constant
            | AWrite Var Expr
            | ARead VarId Var

data ActionAssignment = AAssign Var Expr

data BoolTransitionExpr = BTEVarPrime VarPrime
                        | BTEConstant Constant
                        | BTEObject LocalObject
                        | BTEIn Var VarType
                        | BTEAEq ArithExpr ArithExpr
                        | BTEANeq ArithExpr ArithExpr
                        | BTEEEq EnumExpr EnumExpr
                        | BTEENeq EnumExpr EnumExpr
                        | BTEARel RelOp ArithExpr ArithExpr
                        | BTEERel RelOp EnumExpr EnumExpr
                        | BTEBOp BoolOp BoolTransitionExpr BoolTransitionExpr
                        | BTENeg BoolTransitionExpr
                        | BTEParen BoolTransitionExpr

data Expr = BExpr BoolExpr | AExpr ArithExpr | EExpr EnumExpr

data BoolExpr = BEVarPrime VarPrime
              | BEConstant Constant
              | BEIn Var VarType
              | BEAEq ArithExpr ArithExpr
              | BEANeq ArithExpr ArithExpr
              | BEEEq EnumExpr EnumExpr
              | BEENeq EnumExpr EnumExpr
              | BEARel RelOp ArithExpr ArithExpr
              | BEERel RelOp EnumExpr EnumExpr
              | BEBOp BoolOp BoolTransitionExpr BoolTransitionExpr
              | BENeg BoolTransitionExpr
              | BEParen BoolTransitionExpr

data ArithExpr = AEVarPrime VarPrime
               | AEInt Integer
               | AEOp ArithOp ArithExpr ArithExpr
               | AEParen ArithExpr

data EnumExpr = EEVarPrime VarPrime
              | EEConstant Constant
              | EEPrev EnumExpr
              | EENext EnumExpr
              | EEParen EnumExpr

data VarType = EnumType [Constant]
             | RangeType Integer Integer

data VarDec = VarDec VarId Type

data Type = Observable RawType | Plain RawType

data RawType = RawType TypeId | Array TypeId Integer

data Var = Var VarId | ArraySelf VarId | ArrayAccess VarId Integer

data VarPrime = Primed Var | UnPrimed Var

data LocalObject = LocalObject Constant Constant

data BoolOp = And | Or | Implies | Equiv | Xor

data ArithOp = Plus | Minus

data RelOp = Lt | Lte | Gt | Gte

data EnvSpec = Obs

