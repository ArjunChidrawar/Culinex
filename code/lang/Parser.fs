module Parser
open AST
open Combinator
open System
open System.IO

let pad p = pbetween pws0 p pws0

let ingredient =
    pmany1 pletter
    |>> (fun ls -> stringify(ls))
    |>> (fun f -> Ingredient(f))


let time = 
    pad(pmany1 pdigit)
    |>> (fun ds -> stringify(ds))
    |>> (fun s -> int(s))
    |>> (fun i -> Time(i))

let diet =
    pad(
    pstr "vegetarian" <|>
    pstr "gluten free" <|> 
    pstr "pescatarian") 
    |>> (fun d -> Diet(d))

let recipeList =
    pmany1 (pad (diet <|> ingredient <|> time))

let expr = pad(recipeList) // <|> time <|> diet <|> flavor

let grammar = pleft expr peof

let parse (input: string): RecipeList option =
    let DEBUG = true
    let i = if DEBUG then debug input else prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(pos,rule) -> 
        printfn "Invalid Expression."
        let msg = sprintf "Cannot parse input at position %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos input msg
        printf "%s" diag
        None