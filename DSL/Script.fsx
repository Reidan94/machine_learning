// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @"..\packages\"
#r @"FSharp.Data.2.3.3\lib\net40\FSharp.Data.dll"
// Define your library scripting code here
open FSharp.Data
// Data types that allows strings to contain special symbols
//1.Verbatim strings - doubled "
let verbatimStrings = @"dfgfgfgfg""temp""\rtrt"
//2.Triple braces. Without  doubled "
let triple = """dlfkjdfjhdkfjhdf "Hello world" dfdfdf"""

//type Billing = JsonProvider<"""https://billing.x-plarium.com/Payments/api/prices/v2/all/a36n30g2000""">
//let billing2 = """https://billing.x-plarium.com/Payments/api/prices/v2/all/a36n30g2000"""
//Billing.Load(billing2) |>
//        Seq.iter (fun x -> 
//        printfn "%d" x.Id
//        printfn "%d" x.Dpt
//        printfn "%s" (x.Data.B.ToString())
//        x.Packs|> Seq.iter(fun y-> printfn "\tProduct Id - %s. Price - %f" y.I y.P))

[<Literal>]
let sample = """{"items":[
{"tags":["java","arrays"],"owner":{"reputation":2,"user_id":8289523,"user_type":"registered","profile_image":"https://lh3.googleusercontent.com/-feQ1Pxwpezo/AAAAAAAAAAI/AAAAAAAAFJo/tu334EvMxY8/photo.jpg?sz=128","display_name":"Shivam Tewari","link":"https://stackoverflow.com/users/8289523/shivam-tewari"}},
{"tags":["javascript","jquery","html"],"owner":{"reputation":2,"user_id":8289523,"user_type":"registered","profile_image":"https://lh3.googleusercontent.com/-feQ1Pxwpezo/AAAAAAAAAAI/AAAAAAAAFJo/tu334EvMxY8/photo.jpg?sz=128","display_name":"Shivam Tewari","link":"https://stackoverflow.com/users/8289523/shivam-tewari"}}],
"has_more":true,"quota_max":300,"quota_remaining":299}"""

type hardCodedQuestions = JsonProvider<sample>

[<Literal>]
let javaQuery = "https://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=java"
let javaQuestions = hardCodedQuestions.Load(javaQuery)
printfn "%b" javaQuestions.HasMore
printfn "%d" javaQuestions.QuotaMax
printfn "%d" javaQuestions.QuotaRemaining
javaQuestions.Items|> Seq.iter (fun x-> printfn "%s" x.Owner.DisplayName
                                        x.Tags|> Seq.iter (fun y -> printfn "\t%s" y))

// Small dsl
let questionQuery = """https://api.stackexchange.com/2.2/questions?site=stackoverflow"""
let tagged tags query = 
    //join tags in separated string
    let joinedTags = tags |> String.concat ";"
    sprintf "%s&tagged=%s" query joinedTags

type Questions = JsonProvider<"""https://api.stackexchange.com/2.2/questions?site=stackoverflow""">
let page p query = sprintf "%s&spage=%s" query p
let pageSize s query = sprintf "%s&pagesize=%i" query s
let extractQuestions (query:string) = Questions.Load(query)
let printSamples(sample:Questions.Root) = 
                                     printfn "%i" sample.QuotaMax
                                     printfn "%i" sample.QuotaRemaining
                                     sample.Items|> Seq.iter (fun x->
                                            printfn "Title - %s" x.Title
                                            printfn "Date - %i" x.CreationDate
                                            printfn "Owner name - %s" x.Owner.DisplayName
                                            printfn "\n\n")

// `` delimits identifier that is a keyword
let ``C#`` = "C%23"
let ``F#`` = "F%23"
let fsSample = questionQuery |> tagged [``F#``]|> pageSize 100|> extractQuestions
let csSample = questionQuery |> tagged [``C#``]|> pageSize 120|> extractQuestions

fsSample|> printSamples
csSample|> printSamples

let ``The Price Should Be Positive`` g = g > 0
if ``The Price Should Be Positive`` 20 then printfn "OK" else printfn "NOT OK"

let wb = WorldBankData.GetDataContext()
let pop2000 = [for i in wb.Countries -> i.Indicators.``Population, total``.[2000]]
let pop2010 = [for i in wb.Countries -> i.Indicators.``Population, total``.[2010]]

//let csQuestions = """https://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=C#"""
//Questions.Load(csQuestions).Items|>Seq.iter (fun q-> printfn "%s" q.Title)

