module Compiler.String (
    getStringTable
) where

import Parser.AST (Stmt(..), Expr(..))
import Parser.Type (Type(..))
import Parser.Pattern (Pattern(..))

getFunctions :: [Stmt] -> [String]
getFunctions (FuncDeclStmt name _ _ _ : xs) = name : getFunctions xs
getFunctions (_ : xs) = getFunctions xs
getFunctions [] = []

argsToString :: [(String, Type, Maybe Expr)] -> [String]
argsToString ((var, _, _) : xs) = var : argsToString xs
argsToString [] = []

lambdaArgToString :: [(String, Type)] -> [String]
lambdaArgToString ((str, _) : xs) = str : lambdaArgToString xs
lambdaArgToString [] = []

findExprString :: Expr -> [String]
findExprString (LitString str) = [str]
findExprString (FuncCall e1 e2) = findExprString e1 ++ concatMap findExprString e2
findExprString (BinLogic _ e1 e2) = findExprString e1 ++ findExprString e2
findExprString (BinComp _ exp1 exp2) = findExprString exp1 ++ findExprString exp2
findExprString (BinArith _ exp1 exp2) = findExprString exp1 ++ findExprString exp2
findExprString (UnaryLogic _ exp1) = findExprString exp1
findExprString (Ternary e1 e2 e3) = findExprString e1 ++ findExprString e2 ++ findExprString e3
findExprString (LitArray ex) = concatMap findExprString ex
findExprString (LitTuple ex) = concatMap findExprString ex
findExprString (Assign e1 e2) = findExprString e1 ++ findExprString e2
findExprString (LambdaExpr args _ ex) = lambdaArgToString args ++ findExprString ex
findExprString (BlockExpr ex) = concatMap findExprString ex
findExprString _ = []

findPatterString :: Pattern -> [String]
findPatterString (PatLitString str) = [str]
findPatterString (PatOr pats) = concatMap findPatterString pats
findPatterString (PatRange p1 p2) = findPatterString p1 ++ findPatterString p2
findPatterString (PatRangeInclusive p1 p2) = findPatterString p1 ++ findPatterString p2
findPatterString (PatTuple pats) = concatMap findPatterString pats
findPatterString (PatArray pats) = concatMap findPatterString pats
findPatterString (PatVar str) = [str]
findPatterString _ = []

findMatchString :: [(Pattern, Stmt)] -> [String]
findMatchString ((pat, stmt) : xs) = findPatterString pat ++ findString [stmt] ++ findMatchString xs
findMatchString [] = []

findString :: [Stmt] -> [String]

findString (LetStmt var _ (LitString str):xs) = var:str:findString xs
findString (LetStmt var _ _:xs) = var:findString xs

findString (ReturnStmt (Just ex) : xs) = findExprString ex ++ findString xs

findString (BlockStmt stmts : xs) = findString stmts ++ findString xs

findString (ExprStmt ex : xs) = findExprString ex ++ findString xs

findString (IfStmt ex s1 (Just s2) : xs) = findExprString ex ++ findString [s1] ++ findString [s2] ++ findString xs
findString (IfStmt ex s1 Nothing : xs) = findExprString ex ++ findString [s1] ++ findString xs

findString (ForRangeStmt str e1 e2 _ (Just e3) stmts : xs) = str : findExprString e1 ++ findExprString e2 ++ findExprString e3 ++ findString stmts ++ findString xs
findString (ForRangeStmt str e1 e2 _ Nothing stmts : xs) = str : findExprString e1 ++ findExprString e2 ++ findString stmts ++ findString xs

findString (ForClassicStmt (Just stmt) e1 (Just e2) stmts : xs) = findString [stmt] ++ findExprString e1 ++ findExprString e2 ++ findString stmts ++ findString xs
findString (ForClassicStmt Nothing e1 (Just e2) stmts : xs) = findExprString e1 ++ findExprString e2 ++ findString stmts ++ findString xs
findString (ForClassicStmt (Just stmt) e1 Nothing stmts : xs) = findString [stmt] ++ findExprString e1 ++ findString stmts ++ findString xs

findString (WhileStmt ex stmts : xs) = findExprString ex ++ findString stmts ++ findString xs

findString (MatchStmt ex patterns : xs) = findExprString ex ++ findMatchString patterns ++ findString xs

findString (FuncDeclStmt _ args _ (Just body) : xs) = argsToString args ++ findString [body] ++ findString xs
findString (FuncDeclStmt _ args _ Nothing : xs) = argsToString args ++ findString xs


findString (_:xs) = findString xs
findString [] = []


getStringTable :: [Stmt] -> [String]
getStringTable ast = getFunctions ast ++ findString ast
