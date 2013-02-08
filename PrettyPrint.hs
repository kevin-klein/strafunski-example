module PrettyPrint where

import Text.PrettyPrint.HughesPJ hiding (Str)
import PyAst 

ppModule :: Module -> Doc
-- other constructors than this should not occur
ppModule (Module stmts pos) = 
    ppStmts stmts

-------------Arguments----------------------------------------------------------

ppArguments (Arguments args vararg kwarg defaults) =
    ppArg args <+> ppVarArg vararg <+> ppKwArg kwarg <+> ppDefaultArgs defaults

ppArg [] = empty
ppArg [x] = ppExpr x
ppArg (x:xs) =
    ppExpr x <> text "," <+> ppArg xs

ppVarArg Nothing = empty
ppVarArg (Just id) =
    text "*" <> text id

ppKwArg Nothing = empty
ppKwArg (Just id) =
    text "**" <> text id

ppDefaultArgs [] = empty
ppDefaultArgs (x:xs) =
    ppExpr x <+> ppDefaultArgs xs

--------------------------------------------------------------------------------

ppStmt (FunctionDef id args body decorators _) =
    ppDecorators decorators 
    $+$ text "def" <+> text id <> parens (ppArguments args) <> text ":" $+$ nest 4 (ppStmts body) 

    where
        ppDecorators [] = empty
        ppDecorators [x] = text "@" <> ppExpr x
        ppDecorators (x:xs) = text "@" <> ppExpr x $+$ ppDecorators xs

ppStmt (For target iter body orelse _) =
    text "for" <+> ppExpr target <+> text "in" <+> ppExpr iter <> text ":"
    $+$ nest 4 (ppStmts body) $+$ ppOrElse orelse
    where
        ppOrElse [] = empty
        ppOrElse x = text "else" $+$ nest 4 (ppStmts x)

ppStmt (Expr expr _) = ppExpr expr

ppStmt (Assign targets value _) =
    ppTargets targets <+> text "=" <+> ppExpr value
    where
        ppTargets [] = empty
        ppTargets [x] = ppExpr x
        ppTargets (x:xs) = ppExpr x <> text "," <+> ppTargets xs

ppStmt (Return expr _) = 
    text "return" <+> ppReturnExpr expr
    where
        ppReturnExpr Nothing = empty
        ppReturnExpr (Just x) = ppExpr x

ppStmt (AugAssign target op value _) =
    ppExpr target <+> ppOperator op <> text "=" <+> ppExpr value
    
ppStmt (If cmp body orelse _) =
    text "if" <+> ppExpr cmp <> text ":" $+$ nest 4 (ppStmts body) $+$ ppElse orelse
    
    where
        ppElse [] = empty
        ppElse xs = text "else" $+$ nest 4 (ppStmts xs)

--------------------------------------------------------------------------------

ppStmts [] = empty
ppStmts [x] = ppStmt x
ppStmts (x:xs) = ppStmt x $+$ ppStmts xs

--------------------------------------------------------------------------------

ppExpr (BoolOp op values) =
    ppBoolOp' values
    where
        ppBoolOp' [] = empty
        ppBoolOp' (x:xs) =
            ppExpr x <+> ppBoolOp op <+> ppBoolOp' xs

ppExpr (BinOP left op right) =
    ppExpr left <+> ppOperator op <+> ppExpr right

ppExpr (UnaryOp unaryOp expr) =
    ppUnaryOp unaryOp <> ppExpr expr

ppExpr (Lambda arguments expr) =
    text "lambda" <+> ppArguments arguments <> text ":" <+> ppExpr expr

ppExpr (IfExp cond body else_body) =
    ppExpr body <+> text "if" <+> ppExpr cond <+> text "else" <+> ppExpr else_body

ppExpr (Dict keys values) =
    let zs = zip keys values in
    text "{" <> ppKeyValuePair zs <> text "}"
    where
        ppKeyValuePair [] = empty
        ppKeyValuePair (x:xs) =
            case x of
                (key, value) -> 
                    ppExpr key <+> text ":" <+> ppExpr value <+> text "," <+> ppKeyValuePair xs

ppExpr (Set elts) =
    text "{" <> ppElts elts <> text "}"
    where
        ppElts [] = empty
        ppElts [e] = ppExpr e
        ppElts (x:xs) = ppExpr x <> text ";" <+> ppElts xs

ppExpr (ListComp elt generators) =
    text "[" <> ppExpr elt <+> ppComprehensions generators <> text "]"

ppExpr (SetComp elt generators) =
    text "{" <> ppExpr elt <+> ppComprehensions generators <> text "}"

ppExpr (DictComp key value generators) =
    text "{" <> ppExpr key <> text ":" <> ppExpr value <+> ppComprehensions generators <> text "}"

ppExpr (Yield val) =
    case val of
        Just x -> text "yield" <+> ppExpr x

        Nothing -> text "yield"

ppExpr (Compare left ops comparators) =
    let ops_and_comps = zip ops comparators in
    ppExpr left <+> ppCompRest ops_and_comps
    where
        ppCompRest [] = empty
        ppCompRest [(op, comp)] = ppCmpOp op <+> ppExpr comp
        ppCompRest ((op, comp):xs) = ppCmpOp op <+> ppExpr comp <+> ppCompRest xs

ppExpr (Call func args keywords starargs kwargs) =
    ppExpr func <> parens (ppArgs args <> ppKeywords keywords <> ppStarargs starargs <> ppKwargs kwargs) 

ppExpr (Repr value) = error "no idea what that is"

ppExpr (Num n) = float n

ppExpr (Str s) = text "\"" <> text s <> text "\""

ppExpr (Attribute value attr _) =
    ppExpr value <> text "." <> text attr

ppExpr (Subscript value slice _) =
    ppExpr value <> text "[" <> ppSlice slice <> text "]"

ppExpr (Name id _) = text id

ppExpr (List elts _) =
    text "[" <> ppList elts <> text "]"
    where
        ppList [] = empty
        ppList [x] = ppExpr x
        ppList (x:xs) = ppExpr x <> text "," <+> ppList xs

ppExpr (Tuple elts _) = 
    parens ( ppList elts )
    where
        ppList [] = empty
        ppList [x] = ppExpr x
        ppList (x:xs) = ppExpr x <> text "," <+> ppList xs 

--------------------------------------------------------------------------------

ppArgs :: [Expr] -> Doc
ppArgs [] = empty
ppArgs [x] = ppExpr x
ppArgs (x:xs) = ppExpr x <> text "," <+> ppArgs xs

ppKeywords [] = empty
ppKeywords [(Keyword arg value)] = 
    text "," <+> text arg <+> text "=" <+> ppExpr value
ppKeywords ((Keyword arg value):xs) =
    text "," <+> text arg <+> text "=" <+> ppExpr value <+> ppKeywords xs

ppStarargs Nothing = empty
ppStarargs (Just expr) = text "*" <> ppExpr expr

ppKwargs Nothing = empty
ppKwargs (Just expr) = text "**" <> ppExpr expr

--------------------------------------------------------------------------------

ppSlice (Ellipsis) = empty
ppSlice (Slice lower upper step) =
    text "[" <> ppLower lower <> ppUpper upper <> ppStep step <> text "]"
    where
        ppLower Nothing = empty
        ppLower (Just x) =
            ppExpr x <> text ":"

        ppUpper Nothing = empty
        ppUpper (Just x) =
            ppExpr x

        ppStep Nothing = empty
        ppStep (Just x) =
            text ":" <> ppExpr x

ppSlice (ExtSlice dims) =
    ppSlice' dims
    where
        ppSlice' [] = empty
        ppSlice' [x] = ppSlice x
        ppSlice' (x:xs) = ppSlice x <+> ppSlice' xs

ppSlice (Index expr) = 
    text "[" <> ppExpr expr <> text "]"

--------------------------------------------------------------------------------

ppComprehensions [] = empty
ppComprehensions (x:xs) = 
    ppComprehension x <+> ppComprehensions xs

ppComprehension (Comprehension target iter ifs) =
    text "for" <+> ppExpr target <+> text "in" <+> ppExpr iter <+> ppIfs ifs
    where
        ppIfs [] = empty
        ppIfs [x] = text "if" <+> ppExpr x
        ppIfs (x:xs) = text "if" <+> ppExpr x <+> ppIfs xs

--------------------------------------------------------------------------------

ppCmpOp Eq = text "=="
ppCmpOp NotEq = text "!="
ppCmpOp Lt = text "<"
ppCmpOp LtE = text "<="
ppCmpOp Gt = text ">"
ppCmpOp GtE = text ">="
ppCmpOp Is = text "is"
ppCmpOp IsNot = text "is not"
ppCmpOp In = text "in"
ppCmpOp NotIn = text "not in"

ppBoolOp And = text "and"
ppBoolOp Or = text "or"

ppUnaryOp Invert = text "~"
ppUnaryOp Not = text "not"
ppUnaryOp UAdd = text "+"
ppUnaryOp USub = text "-" 

ppOperator Add = text "+"
ppOperator Sub = text "-"
ppOperator Mult = text "*"
ppOperator Div = text "/"
ppOperator Mod = text "**"
ppOperator LShift = text "<<"
ppOperator RShift = text ">>"
ppOperator BitOr = text "|"
ppOperator BitAnd = text "&"
ppOperator BitXor = text "^"
ppOperator FloorDiv = text "//"

--------------------------------------------------------------------------------
    
