open Combinator
open AST
open Parser
open CS334
open System.IO
open System.Collections.Generic
open FSharp.Data


let readInput(input: string) =
    if File.Exists input then
        let i = File.ReadAllLines input
        i|> String.concat " "
    else
        input

[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn "Usage: dotnet run \"<ingredient list>\" \n "
        exit 1
    let input = readInput args[0]
    let ast_maybe = parse input
    match ast_maybe with
    | Some ast ->
        let resList = eval ast []
        if resList.Length < 1 then
            printfn "Unfortunately there are no recipes that match your preferences \nMaybe try another search?"
            exit 1
        else
            // printfn "candidate recipes: %A" resList
            let res = returnRecipe resList
            printfn "%A" res
    | None ->
        printfn "Usage: dotnet run \"<ingredient list>\" \nIngredients must be valid"
    0

