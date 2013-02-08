module Refactor where

import Analyse
import Control.Monad
import StrategyLib hiding (replaceFocus)
import PyAst
import Data.Maybe
import Debug.Trace

rewriteToMap :: Stmt -> [Stmt]
rewriteToMap for@(For target iter body orelse pos) =
    case body of
        [Expr (Call (Attribute (Name listname Load) "append" Load) args@[x] [] Nothing Nothing) _] ->
            let lambda_args = Arguments [target] Nothing Nothing [] in
            [AugAssign (Name listname Store) Add (Call (Name "map" Load) [(Lambda lambda_args x), iter] [] Nothing Nothing) pos]
        
        _ ->
            case (last body) of
                (Expr (Call (Attribute (Name listname Load) "append" Load) args@[x] [] Nothing Nothing) _) -> 
                    let arguments = Arguments [target] Nothing Nothing [] in
                    [
                         FunctionDef "_map_func_" arguments ((init body) ++ [Return (Just $ head args) pos]) [] pos,
                         AugAssign (Name listname Store) Add (Call (Name "map" Load) [(Name "_map_func_" Load), iter] [] Nothing Nothing) pos
                         ]

                _ -> [for]

rewriteToFilter :: [Stmt] -> [Stmt]
rewriteToFilter [] = []

rewriteToFilter [for@(For target iter body orelse pos)] =
    case body of
        [If test [Expr (Call (Attribute (Name listname Load) "append" Load) args@[x] [] Nothing Nothing) _] [] pos] ->
            let lambda_args = Arguments args Nothing Nothing [] in
            [AugAssign (Name listname Store) Add (Call (Name "filter" Load) [(Lambda lambda_args test), iter] [] Nothing Nothing) pos]
        
        _ ->
            case (last body) of
                (If test [Expr (Call (Attribute (Name listname Load) "append" Load) args@[x] [] Nothing Nothing) _] [] pos) -> 
                    let arguments = Arguments [target] Nothing Nothing [] in
                    [
                         FunctionDef "_filter_func_" arguments ((init body) ++ [Return (Just $ test) pos]) [] pos,
                         AugAssign (Name listname Store) Add (Call (Name "filter" Load) [(Name "_filter_func_" Load), iter] [] Nothing Nothing) pos
                         ]

                _ -> [for]

rewriteToFilter l = l

replaceFocus :: (MonadPlus m) => Module -> [(Stmt, [Stmt])] -> m Module
replaceFocus mod focus =
    applyTP (once_tdTP (failTP `adhocTP` rewrite)) mod
    where
        rewrite :: (MonadPlus m) => [Stmt] -> m [Stmt]
        rewrite [] = return []
        rewrite (x:xs) = 
                case equalFocus focus x of
                    Just t -> do
                        r <- rewrite xs
                        return (t ++ r)
                    _ -> do
                        r <- (rewrite xs)
                        return ([x] ++ r)
            where
                equalFocus :: (MonadPlus m) => [(Stmt, [Stmt])] -> Stmt -> m [Stmt]
                equalFocus [] o = mzero
                equalFocus ((orig, focus):xs) o =
                    if orig == o
                        then return focus
                        else equalFocus xs o
            
            
refactor :: (MonadPlus m) => Module -> m Module
refactor ast = do
    fors <- findForLoops ast
    let legalFors = filter (isLegalForRefactoring ast) fors
    
    let mapFors = map rewriteToMap legalFors
    let filterFors = map rewriteToFilter mapFors
    
    -- put focus back in to the ast
    m <- replaceFocus ast $ zip legalFors filterFors
    
    return m

