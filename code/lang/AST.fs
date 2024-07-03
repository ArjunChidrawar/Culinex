module AST
open Combinator

type Expr = 
    | Ingredient of string
    | Time of int
    | Diet of string
type RecipeList = Expr list