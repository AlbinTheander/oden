module Oden.Simplify (simplifyPackage) where

import Oden.Core
import Oden.Core.Operator
import Oden.SourceInfo

simplifyPackage :: Package -> Package
simplifyPackage (Package declaration imports definitions) =
  Package declaration imports (map simplifyDefinition definitions)

simplifyDefinition :: Definition -> Definition
simplifyDefinition (Definition si name (scheme, expr)) = Definition si name (scheme, simplifyExpr expr)
simplifyDefinition d = d

simplifyExpr :: Expr t -> Expr t
simplifyExpr (UnaryOp si op e t) = simplifyUnaryOp si op e t
simplifyExpr (BinaryOp si op e1 e2 t) = simplifyBinaryOp si op e1 e2 t
simplifyExpr (UncurriedFnApplication si e es t) =
  (UncurriedFnApplication si (simplifyExpr e) (map simplifyExpr es) t)
simplifyExpr (NoArgFn si e t) = (NoArgFn si (simplifyExpr e) t)
simplifyExpr (Slice si es t) = (Slice si (map simplifyExpr es) t)
simplifyExpr x = x


simplifyUnaryOp :: SourceInfo -> UnaryOperator -> (Expr t) -> t -> (Expr t)
simplifyUnaryOp si op e t = 
  case (op, simpleE) of
    (Negative, (Literal _ (Int v) t)) -> (Literal si (Int (-v)) t)
    _                                 -> (UnaryOp si op e t)
  where
    simpleE = simplifyExpr e

simplifyBinaryOp :: SourceInfo -> BinaryOperator -> (Expr t) -> (Expr t) -> t -> (Expr t)
simplifyBinaryOp si op e1 e2 t =
  case (op, simpleE1, simpleE2) of
    (Subtract, (Literal _ (Int v1) _), (Literal _ (Int v2) _)) -> (Literal si (Int (v1 - v2)) t)
    _                                                          -> (BinaryOp si op e1 e2 t)
  where
    simpleE1 = simplifyExpr e1
    simpleE2 = simplifyExpr e2

