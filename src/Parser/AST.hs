module Parser.AST (Expr(..), Stmt(..), ArithOp(..), CompOp(..), LogicOp(..), UnaryOp(..), RangeType(..)) where

import Parser.Type (Type)
import Parser.Pattern (Pattern)

data Expr
    = LitInt Int
    | LitString String
    | LitBool Bool
    | Var String
    | FuncCall String [Expr]
    | BinArith ArithOp Expr Expr
    | BinComp CompOp Expr Expr
    | BinLogic LogicOp Expr Expr
    | UnaryLogic UnaryOp Expr
    | Ternary Expr Expr Expr
    | LitArray [Expr]
    | LitTuple [Expr]
    | Assign Expr Expr
    | ArrayIndex Expr Expr
    | LambdaExpr [(String, Type)] Type Expr
    | BlockExpr [Expr]
    deriving (Show, Eq)

data ArithOp = Add | Subtract | Multiply | Divide | Modulo deriving (Show, Eq)
data CompOp = Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual deriving (Show, Eq)
data LogicOp = And | Or deriving (Show, Eq)
data UnaryOp = Not deriving (Show, Eq)

data Stmt
    = LetStmt String (Maybe Type) Expr
    | ReturnStmt (Maybe Expr)
    | BlockStmt [Stmt]
    | ExprStmt Expr
    | IfStmt Expr Stmt (Maybe Stmt)
    | ForRangeStmt String Expr Expr RangeType (Maybe Expr) [Stmt]
    | ForClassicStmt (Maybe Stmt) Expr (Maybe Expr) [Stmt]
    | WhileStmt Expr [Stmt]
    | DoWhileStmt [Stmt] Expr
    | BreakStmt
    | ContinueStmt
    | MatchStmt Expr [(Pattern, Stmt)]
    | FuncDeclStmt String [(String, Type, Maybe Expr)] Type (Maybe Stmt)
    deriving (Show, Eq)

data RangeType = Exclusive | Inclusive
    deriving (Show, Eq)

{--

2.1. Contexte d'utilisation
BlockStmt : Utilisé dans des instructions où une séquence d'instructions est nécessaire. Par exemple, dans les boucles for, les conditions if, ou dans les déclarations de fonctions.
BlockExpr : Utilisé lorsqu'une séquence d'expressions est nécessaire, par exemple dans des lambdas ou des expressions complexes.

--}