{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NPlusKPatterns #-}

module Analyse where

import StrategyLib
import PyAst
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad

anonid = "$"
startid = "$start_from_here$"
startName = Name startid Load


-- legality check
findNames :: (MonadPlus m, Term t) => t -> m [Expr]
findNames =
    -- clear functions first
    applyTU (full_tdTU step)
    where
        step = constTU [] `adhocTU` matchFunction `adhocTU` matchName
        
matchName :: (MonadPlus m) => Expr -> m [Expr]        
matchName n@(Name _ _) = return [n]
matchName _ = return []
        
matchFunction :: (MonadPlus m) => Stmt -> m [Expr]
matchFunction (FunctionDef id (Arguments a _ _ _) _ _ _) = return $ [(Name id Store)] ++ a
matchFunction _ = return []

findIllegalNames :: [Expr] -> [String]
findIllegalNames names = 
    findIllegalNames' names [] []
    where
        findIllegalNames' [] defined undefined =
            defined `intersect` undefined
            
        findIllegalNames' (x:xs) defined undefined =
            case x of
                (Name id Store) ->
                    findIllegalNames' xs (nub $ defined ++ [id]) (nub $ undefined)
                (Name id Load) ->
                    if id `elem` defined 
                        then findIllegalNames' xs defined undefined
                        else findIllegalNames' xs defined (nub $ undefined ++ [id]) 
                                                
                (Name id Del) ->
                    if id `elem` defined
                        then findIllegalNames' xs (nub $ delete id defined) undefined
                        else findIllegalNames' xs defined undefined
            
                _ ->
                    findIllegalNames' xs defined undefined

replaceFunctions :: (MonadPlus m, Term t) => t -> m t
replaceFunctions = applyTP (innermost (monoTP elimFunction))

elimFunction :: (MonadPlus m) => Stmt -> m Stmt 
elimFunction (FunctionDef id _ _ _ pos) = return $ Assign [(Name id Store)] (Num 42.0) pos
elimFunction _ = mzero
  
isHalfwayPure :: Stmt -> Bool      
isHalfwayPure for = 
    case length illegal of
        0 -> True
        _ -> False
    

    where
        funcfreebody = fromJust $ replaceFunctions for
        names = fromJust $ findNames for
        illegal = findIllegalNames names
    

findFunctions :: (MonadPlus m, Term t) => t -> m [Stmt]
findFunctions = 
    applyTU (full_tdTU step) 
    where 
        step = constTU [] `adhocTU` matchFunction
        matchFunction f@(FunctionDef _ _ _ _ _) = return [f]
        matchFunction _ = return []
    
findContainingFunction :: (MonadPlus m, Term t) => Stmt -> t -> m Stmt
findContainingFunction stmt ast = do

    functions <- findFunctions ast
    let clean_functions = map (\(FunctionDef id args body decorators pos) -> FunctionDef id args (map (fromJust . replaceFunctions) body) decorators pos) functions 
  
    outerFunction <- realContainingFunction clean_functions

    return outerFunction

    case outerFunction of
        [] -> mzero
        [f] ->
            case findIndex (\x -> x == f) functions of
                Just i -> return $ functions !! i
                Nothing -> mzero
        (x:xs) -> error "focus ambigious"
    
    where
        realContainingFunction = applyTU (once_td step)
        
        step = constTU [] `adhocTU` (getFocus stmt)
        
        getFocus :: (MonadPlus m) => Stmt -> Stmt -> m [Stmt]
        getFocus stmt s
            | s == stmt = return [s]
            | otherwise = mzero

findNamesWithStart func focus =
    applyTU (full_buTU step) func
    where
        step = constTU [] `adhocTU` matchName `adhocTU` (failOnFocus focus)
        
        failOnFocus focus s 
            | focus == s = return [startName]
            | otherwise = return []

laterNames :: (MonadPlus m, Term t, Eq t) => Stmt -> t -> m [Expr]
laterNames func focus = do
    func_free <- (case func of
        (FunctionDef _ _ body _ _) -> 
             replaceFunctions body)
   
    names <- findNamesWithStart func_free focus

    return $ tail $ dropWhile (\(Name id _) -> id /= startid) names

findUsedLaterNames :: (MonadPlus m, Term t, Eq t) => Stmt -> t -> m [Expr]
findUsedLaterNames func focus = do
    names <- laterNames func focus
    
    funcs <- replaceFunctions focus
    loop_names <- findNames funcs
    let loop_defined_names = nub $ filter (\(Name _ ctx) -> ctx == Load) loop_names    
    
    return $ filter (isReused loop_defined_names) names
    
    where
        nameCompare (Name id _) (Name id' _) = id == id' 
            
        isReused names (Name id ctx) =
            case occurs of
                [] -> True
                ((Name _ Load):xs) -> True
                _ -> False
            
            where
                occurs = filter (\(Name id' _) -> id == id') names

findForLoops :: (MonadPlus m) => Module -> m [Stmt]
findForLoops =
    applyTU (full_tdTU (constTU [] `adhocTU` isFor))
    where
        isFor x@(For _ _ _ _ _) = return [x]
        isFor _ = return []         
    
isLegalForRefactoring :: Module -> Stmt -> Bool
isLegalForRefactoring ast focus =

    case (isHalfwayPure focus, findUsedLaterNames func focus) of
        (False, _) -> False
        (True, [[]]) -> True
        _ -> False
        
    where
        func = case findContainingFunction focus ast of
            Nothing -> (\(Module stmts _) -> FunctionDef anonid (Arguments [] Nothing Nothing [])  stmts [] (Pos 0 0)) ast
            Just t -> t
    
    


