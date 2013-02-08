import StrategyLib
import Analyse
import PyAst
import Data.Maybe
import Refactor
import PrettyPrint

main = do
    let ast = Module [For (Name "x" Store) (Name "z" Load) [Expr (Call (Attribute (Name "l" Load) "append" Load) [(BinOP (Name "x" Load) Add (Num 1.0))] [] Nothing Nothing ) (Pos 0 0)]  [] (Pos 1 0)] (Pos 0 0)

    let filter_ast = Module [For (Name "x" Store) (Name "z" Load) [Assign [Name "x" Store] (BinOP (Name "x" Load) Add (Num 1.0)) (Pos 0 0),If (Compare (Name "x" Load) [Lt]  [Num 6.0]) [Expr (Call (Attribute (Name "l" Load) "append" Load) [(Name "x" Load)] [] Nothing Nothing ) (Pos 0 0)] [] (Pos 0 0)]  [] (Pos 1 0)] (Pos 0 0)
    
    let unfilter_ast = Module [For (Name "x" Store) (Name "z" Load) [Assign [Name "z" Store] (BinOP (Name "z" Load) Add (Num 1.0)) (Pos 0 0),If (Compare (Name "x" Load) [Lt]  [Num 6.0]) [Expr (Call (Attribute (Name "l" Load) "append" Load) [(Name "x" Load)] [] Nothing Nothing ) (Pos 0 0)] [] (Pos 0 0)]  [] (Pos 1 0)] (Pos 0 0)
    
    let nested_loops = Module [For (Name "x" Store) (Name "y" Load) [For (Name "xi" Store) (Name "yi" Load) [] [] (Pos 0 15)] [] (Pos 0 0)] (Pos 0 0)
    
    putStrLn ""
    putStrLn $ show $ ppModule ast
    putStrLn "refactoring to:"
    print $ ppModule $ fromJust $ refactor ast
    putStrLn ""
    
    print $ ppModule $ filter_ast
    putStrLn "refactoring to:"
    putStrLn $ show $ ppModule $ fromJust $ refactor filter_ast
    
    putStrLn ""
    putStrLn "unrefactorable filter due to state:"
    print $ ppModule $ unfilter_ast
    putStrLn $ show $ ppModule $ fromJust $ refactor unfilter_ast
    
    putStrLn ""
    print $ ppModule $ nested_loops
    putStrLn $ show $ ppModule $ fromJust $ refactor nested_loops
