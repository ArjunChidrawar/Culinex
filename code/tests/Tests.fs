namespace tests


open Parser
open AST
open FSharp.Data
open Program
open CS334
open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestRecipeFind() =
        let input = "chicken cheese carrot"
        let ast_maybe = parse input
        match ast_maybe with
        | Some ast ->
            let resList = eval ast []
            let expected = "You should make \"French Onion Chicken with Roasted Carrots and Mashed Potatoes\"\nHere is a recipe: \"https://www.themealdb.com/meal/52996-French-Onion-Chicken-with-Roasted-Carrots-&-Mashed-Potatoes\" \n This recipe takes 70 minutes to make"
 
            let actual = returnRecipe resList
            Assert.AreEqual(expected, actual)
        | None ->
            printfn "Usage: dotnet run \"<ingredient list>\" \nIngredients must be valid"
    
    [<TestMethod>]
    member this.TestParseFunction() =
        let input = "carrot vegetarian 20"
        let expected = "Some([Ingredient \"carrot\"; Diet \"vegetarian\"; Time 20])"
        let actual = string (parse input)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestDietTimeFuncs() =
        let input = "vegetarian 35"
        let ast_maybe = parse input
        match ast_maybe with
        | Some ast ->
            let resList = eval ast []
            let expected = "You should make \"Creamy Tomato Soup\"\nHere is a recipe: \"https://www.themealdb.com/meal/52841-Creamy-Tomato-Soup\" \n This recipe takes 35 minutes to make"
 
            let actual = returnRecipe resList
            Assert.AreEqual(expected, actual)
        | None ->
            printfn "Usage: dotnet run \"<ingredient list>\" \nIngredients must be valid"
        
