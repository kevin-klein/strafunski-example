{-# OPTIONS_GHC -XDeriveDataTypeable #-}

module PyAst
    (Module(..),
     Stmt(..),
     Expr(..),
     ExprContext(..),
     Slice(..),
     BoolOperator(..),
     Operator(..),
     UnaryOp(..),
     CmpOp(..),
     Comprehension(..),
     ExceptHandler(..),
     Arguments(..),
     Keyword(..),
     Alias(..),
     Identifier(..),
     Pos(..))

 where

import Data.Generics

data Pos = Pos Int Int deriving (Show, Data, Typeable, Eq)

data Module =
    Module [Stmt] Pos
    | Interactive [Stmt] Pos
    | Expression Expr Pos

    deriving (Show, Data, Typeable, Eq)

data Stmt = 
    FunctionDef Identifier Arguments [Stmt] [Expr] Pos
    | ClassDef Identifier  [Expr]  [Stmt]  [Expr] Pos
    | Return  (Maybe Expr) Pos

    | Delete  [Expr] Pos
    | Assign  [Expr] Expr Pos
    | AugAssign    Expr   Operator   Expr Pos

    | Print    (Maybe Expr)   [Expr]   Bool Pos

    | For    Expr   Expr   [Stmt]   [Stmt] Pos
    | While    Expr   [Stmt]   [Stmt] Pos
    | If    Expr   [Stmt]   [Stmt] Pos
    | With    Expr   (Maybe Expr)   [Stmt] Pos

    | Raise    (Maybe Expr)   (Maybe Expr)   (Maybe Expr) Pos
    | TryExcept    [Stmt]   [ExceptHandler]   [Stmt] Pos
    | TryFinally    [Stmt]   [Stmt] Pos
    | Assert    Expr   (Maybe Expr) Pos

    | Import  [Alias] Pos
    | ImportFrom   (Maybe Identifier)   [Alias]   (Maybe Int) Pos

    | Exec   Expr   (Maybe Expr)   (Maybe Expr) Pos

    | Global    [Identifier] Pos
    | Expr    Expr Pos
    | Pass Pos   | Break Pos   | Continue  Pos 
    deriving (Show, Data, Typeable, Eq) 

data Expr = BoolOp    BoolOperator   [Expr]
    | BinOP    Expr   Operator   Expr
    | UnaryOp    UnaryOp   Expr
    | Lambda    Arguments   Expr
    | IfExp Expr Expr Expr
    | Dict    [Expr]   [Expr]
    | Set   [Expr]
    | ListComp    Expr   [Comprehension]
    | SetComp    Expr   [Comprehension]
    | DictComp  Expr Expr [Comprehension]
    | GeneratorsExpr    Expr   [Comprehension]

    | Yield (Maybe Expr)

    | Compare Expr [CmpOp] [Expr]
    | Call Expr [Expr] [Keyword] (Maybe Expr) (Maybe Expr)
    | Repr Expr
    | Num Float
    | Str String    


    | Attribute  Expr  Identifier   ExprContext
    | Subscript    Expr   Slice   ExprContext
    | Name    Identifier   ExprContext
    | List   [Expr]   ExprContext
    | Tuple   [Expr]   ExprContext

    deriving (Show, Data, Typeable, Eq)

data ExprContext = Load | Store | Del | AugLoad | AugStore | Param deriving (Eq, Show, Data, Typeable)

data Slice = Ellipsis
    | Slice    (Maybe Expr)   (Maybe Expr)   (Maybe Expr) 
    | ExtSlice    [Slice] 
    | Index    Expr 
    deriving (Show, Data, Typeable, Eq)

data BoolOperator = And | Or deriving (Show, Data, Typeable, Eq)
 
data Operator = Add | Sub | Mult | Div | Mod | Pow | LShift
    | RShift | BitOr | BitXor | BitAnd | FloorDiv
    deriving (Show, Data, Typeable, Eq)

data UnaryOp = Invert | Not | UAdd | USub deriving (Show, Data, Typeable, Eq)

data CmpOp = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn deriving (Show, Data, Typeable, Eq)

data Comprehension = Comprehension    Expr   Expr   [Expr] deriving (Show, Data, Typeable, Eq)

data ExceptHandler = ExceptHandler   (Maybe Expr) (Maybe Expr)  [Stmt]   deriving (Show, Data, Typeable, Eq)

data Arguments = Arguments    [Expr]   (Maybe Identifier)   (Maybe Identifier)   [Expr] deriving (Show, Data, Typeable, Eq)

data Keyword = Keyword    Identifier   Expr deriving (Show, Data, Typeable, Eq)

data Alias = Alias    Identifier   (Maybe Identifier) deriving (Show, Data, Typeable, Eq)

type Identifier = String


    
