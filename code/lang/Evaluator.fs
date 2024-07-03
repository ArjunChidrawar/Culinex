module CS334

open Combinator
open AST
open Parser
open System
open System.IO
open FSharp.Data
#nowarn "25"

let rec findIngredient file ing  =
    match file with
    | y::[] ->
        if y = ing then ing
        else
            if ing = "vegetarian" || ing = "pescatarian" || ing = "gluten free" then
                printfn "Error: please first specify ingredients then diet preferences"
                exit 1
            else
                printfn "Error: could not find the ingredient '%s'. Please add your ingredient to 'Ingredients.txt'" ing
                exit 1
    | y::ys -> 
        if y = ing then ing
        else
            findIngredient ys ing

let evalIng ingr=
    let readFile path =
        let text = File.ReadAllText(path)
        text.Split('\n')
    let relativePath = Path.Combine(__SOURCE_DIRECTORY__, "Ingredients.txt")
    let x = readFile relativePath
    let ingrList = x |> Array.toList
    let food = findIngredient ingrList ingr
    true

let relative_path = Path.Combine(__SOURCE_DIRECTORY__, "334Recipes.csv")
type file = CsvProvider<"334Recipes.csv">
let recipes = file.Load(relative_path)

let findRecipe (item) (matches) =
    let mutable curr = matches
    for row in recipes.Rows do
        let elems = row.Ingredients.Trim('[', ']').Split(',') |> Array.toList
        if List.contains item elems then
            curr <- curr@[(row.Name, row.Time, row.Ingredients)]
        else
            ()
    curr        
    
// adapted mode function found on stackoverflow: https://stackoverflow.com/questions/50262235/calculate-the-mode-of-a-list-of-integers
let mode (lst) =
    let normalizedCounts =
        lst
        |> Seq.groupBy (fun s -> s)
        |> Seq.map (fun (x, xs) -> (x, Seq.length xs))
    let maxCount =
        normalizedCounts
        |> Seq.map snd
        |> Seq.max
    normalizedCounts
    |> Seq.filter (fun (_, count) -> count = maxCount)
    |> Seq.map fst

let returnRecipe (resList: (string *int*string) list) =
    let rnd = System.Random()
    let mutable newMatches = []
    for entry in resList do
        match entry with
        |(first,_,_) -> 
            // printfn "%s" first
            newMatches <- newMatches@[first]
    // printfn "%A" newMatches
    
    let names = Seq.toList (mode newMatches)
    
    let target = rnd.Next(0, Seq.length names)
    let recipeName = names[target]
    let name = sprintf "You should make %A" recipeName
    let mutable result = ""
    for row in recipes.Rows do
        if row.Name = recipeName then
            result <- sprintf "You should make %A\nHere is a recipe: %A \n This recipe takes %A minutes to make" recipeName row.Recipe row.Time
        else
            ()
    result

//Precondition: time must be specified after the list of ingredients
let filterTime (time:int) (matchList:(string*int*string) list) = 
        if matchList.Length <> 0 then //filter out all recipes with time constraint
            let filtered = List.filter (fun (_,y,_) -> (y <= time)) matchList
            filtered
        else //provide all recipes in the database with the time constraint
            let mutable timeList:(string*int*string) list = []
            for row in recipes.Rows do
                if row.Time <= time  then
                    timeList <- timeList@[(row.Name, row.Time, row.Ingredients)]
                else
                    ()
            timeList   
        
let matchDiet (d:string) (matchList:(string*int*string) list) =
    if matchList.Length <> 0 then
        match d with
        | "vegetarian" ->
            let veg = ["chicken"; "beef"; "pork"; "bacon"; "steak"; "fish"; "salmon"]
            let isVeg candidates = List.forall(fun ing -> not (List.exists (fun nonVegIng -> nonVegIng = ing) veg)) candidates
            let vegRecipes = matchList|> List.filter (fun (_,_,(x:string)) -> 
                let elems = x.Trim('[', ']').Split(',') |> Array.toList
                isVeg elems) 
            vegRecipes
        | "pescatarian" -> 
            let nonPesc = ["chicken"; "beef"; "pork"; "bacon"; "steak"]
            let isPesc candidates = List.forall(fun ing -> not (List.exists (fun nonVegIng -> nonVegIng = ing) nonPesc)) candidates
            let pescRecipes = matchList|> List.filter (fun (_,_,(x:string)) -> 
                let elems = x.Trim('[', ']').Split(',') |> Array.toList
                isPesc elems) 
            pescRecipes
        | "gluten free" -> 
            let glutf = ["bread"; "pasta"]
            let isGlutf candidates = List.forall(fun ing -> not (List.exists (fun nonVegIng -> nonVegIng = ing) glutf)) candidates
            let glutfRecipes = matchList|> List.filter (fun (_,_,(x:string)) -> 
                let elems = x.Trim('[', ']').Split(',') |> Array.toList
                isGlutf elems)
            glutfRecipes
    else
        let mutable matchList:(string*int*string) list = []
        for row in recipes.Rows do
            matchList <- matchList@[(row.Name, row.Time, row.Ingredients)]
        match d with
        | "vegetarian" ->
            let veg = ["chicken"; "beef"; "pork"; "bacon"; "steak"; "lamb"; "duck"; "turkey"; "mutton"; "veal"; "goat"; "elk"; "rabbit"; "fish"; "salmon"; "lobster"; "octopus"; "tuna"; "clam"; "cod"]
            let isVeg candidates = List.forall(fun ing -> not (List.exists (fun nonVegIng -> nonVegIng = ing) veg)) candidates
            let vegRecipes = matchList|> List.filter (fun (_,_,(x:string)) -> 
                let elems = x.Trim('[', ']').Split(',') |> Array.toList
                isVeg elems) 
            vegRecipes
        | "pescatarian" -> 
            let nonPesc = ["chicken"; "beef"; "pork"; "bacon"; "steak"; "lamb"; "duck"; "turkey"; "mutton"; "veal"; "goat"; "elk"; "rabbit"]
            let isPesc candidates = List.forall(fun ing -> not (List.exists (fun nonVegIng -> nonVegIng = ing) nonPesc)) candidates
            let pescRecipes = matchList|> List.filter (fun (_,_,(x:string)) -> 
                let elems = x.Trim('[', ']').Split(',') |> Array.toList
                isPesc elems) 
            pescRecipes
        | "gluten free" -> 
            let glutf = ["bread"; "pasta"; "donut"; "cake"; "cookie"; "flour"]
            let isGlutf candidates = List.forall(fun ing -> not (List.exists (fun nonVegIng -> nonVegIng = ing) glutf)) candidates
            let glutfRecipes = matchList|> List.filter (fun (_,_,(x:string)) -> 
                let elems = x.Trim('[', ']').Split(',') |> Array.toList
                isGlutf elems)
            glutfRecipes
        
let rec eval (e: Expr list) (recipeList:(string*int*string) list) =
    match e with
    | [] -> recipeList
    | f::fs ->
        match f with
        | Ingredient(s) -> 
            if evalIng s then
                let  RecipesI = findRecipe s recipeList
                eval (fs) (RecipesI)
            else
                printfn "failed"
                exit 1
        | Time n -> 
            let filteredT = (filterTime n recipeList)
            eval (fs) (filteredT)
        | Diet d -> 
            let filteredD = matchDiet d recipeList
            eval (fs) (filteredD)