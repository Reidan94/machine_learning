// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @"..\packages\"
#r @"FSharp.Data.2.3.3\lib\net40\FSharp.Data.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.Runtime.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.dll"
#r @"Deedle.1.2.5\lib\net40\Deedle.dll"
#r @"R.NET.Community.1.6.5\lib\net40\RDotNet.dll"
#r @"Deedle.RPlugin.1.2.5\lib\net40\Deedle.RProvider.Plugin.dll"
open FSharp.Data
open RProvider
open RProvider.``base``
open RProvider.graphics
open RProvider.rworldmap
open Deedle
//// Data types that allows strings to contain special symbols
////1.Verbatim strings - doubled "
//let verbatimStrings = @"dfgfgfgfg""temp""\rtrt"
////2.Triple braces. Without  doubled "
//let triple = """dlfkjdfjhdkfjhdf "Hello world" dfdfdf"""
//
////type Billing = JsonProvider<"""https://billing.x-plarium.com/Payments/api/prices/v2/all/a36n30g2000""">
////let billing2 = """https://billing.x-plarium.com/Payments/api/prices/v2/all/a36n30g2000"""
////Billing.Load(billing2) |>
////        Seq.iter (fun x -> 
////        printfn "%d" x.Id
////        printfn "%d" x.Dpt
////        printfn "%s" (x.Data.B.ToString())
////        x.Packs|> Seq.iter(fun y-> printfn "\tProduct Id - %s. Price - %f" y.I y.P))
//
//[<Literal>]
//let sample = """{"items":[
//{"tags":["java","arrays"],"owner":{"reputation":2,"user_id":8289523,"user_type":"registered","profile_image":"https://lh3.googleusercontent.com/-feQ1Pxwpezo/AAAAAAAAAAI/AAAAAAAAFJo/tu334EvMxY8/photo.jpg?sz=128","display_name":"Shivam Tewari","link":"https://stackoverflow.com/users/8289523/shivam-tewari"}},
//{"tags":["javascript","jquery","html"],"owner":{"reputation":2,"user_id":8289523,"user_type":"registered","profile_image":"https://lh3.googleusercontent.com/-feQ1Pxwpezo/AAAAAAAAAAI/AAAAAAAAFJo/tu334EvMxY8/photo.jpg?sz=128","display_name":"Shivam Tewari","link":"https://stackoverflow.com/users/8289523/shivam-tewari"}}],
//"has_more":true,"quota_max":300,"quota_remaining":299}"""
//
//type hardCodedQuestions = JsonProvider<sample>
//
//[<Literal>]
//let javaQuery = "https://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=java"
//let javaQuestions = hardCodedQuestions.Load(javaQuery)
//printfn "%b" javaQuestions.HasMore
//printfn "%d" javaQuestions.QuotaMax
//printfn "%d" javaQuestions.QuotaRemaining
//javaQuestions.Items|> Seq.iter (fun x-> printfn "%s" x.Owner.DisplayName
//                                        x.Tags|> Seq.iter (fun y -> printfn "\t%s" y))
//
//// Small dsl
//let questionQuery = """https://api.stackexchange.com/2.2/questions?site=stackoverflow"""
//let tagged tags query = 
//    //join tags in separated string
//    let joinedTags = tags |> String.concat ";"
//    sprintf "%s&tagged=%s" query joinedTags
//
//type Questions = JsonProvider<"""https://api.stackexchange.com/2.2/questions?site=stackoverflow""">
//let page p query = sprintf "%s&spage=%s" query p
//let pageSize s query = sprintf "%s&pagesize=%i" query s
//let extractQuestions (query:string) = Questions.Load(query)
//let printSamples(sample:Questions.Root) = 
//                                     printfn "%i" sample.QuotaMax
//                                     printfn "%i" sample.QuotaRemaining
//                                     sample.Items|> Seq.iter (fun x->
//                                            printfn "Title - %s" x.Title
//                                            printfn "Date - %i" x.CreationDate
//                                            printfn "Owner name - %s" x.Owner.DisplayName
//                                            printfn "\n\n")
//
//// `` delimits identifier that is a keyword
//let ``C#`` = "C%23"
//let ``F#`` = "F%23"
//let fsSample = questionQuery |> tagged [``F#``]|> pageSize 100|> extractQuestions
//let csSample = questionQuery |> tagged [``C#``]|> pageSize 100|> extractQuestions
//
//fsSample|> printSamples
//csSample|> printSamples
//
//let ``The Price Should Be Positive`` g = g > 0
//if ``The Price Should Be Positive`` 20 then printfn "OK" else printfn "NOT OK"

let mutable m = 12
m <- 1212

let wb = WorldBankData.GetDataContext()
let something = series [for c in wb.Countries -> c.Code, c.Indicators.``Adults (ages 15+) newly infected with HIV``.[2010]]
let datagram = frame ["Something", something]
datagram?Code2 <- datagram.RowKeys
let mapV2 = R.joinCountryData2Map(datagram, "ISO3", "Code2")
R.mapCountryData(mapV2, "Something")


for i2 in something.GetObservations() do
    printfn "Observation key - %s" i2.Key
    printfn "Observation value - %f" i2.Value


for i in wb.Regions do
    printfn "Region - %s" i.Name
    if (i.Countries|>Seq.length) > 0 then 
        printfn "Countries in regions:"
        for j in i.Countries do
            printfn "\t%s" j.Name   

let pop9000 = [for i in wb.Countries -> i.Indicators.``Bank capital to total assets (%)``.[2010]]
R.summary(pop9000) |> R.print
let x = [9.0; 8.0;7.0;11.0]
let y = [11.0;9.0;9.0;2.0]
R.plot(x,y)

let pop2000 = [for i in wb.Countries -> i.Indicators.``Population, total``.[2000]]
let pop2010 = [for i in wb.Countries -> i.Indicators.``Population, total``.[2010]]

let surface2000 = [for i in wb.Countries -> i.Indicators.``Surface area (sq. km)``.[2000]]
let surface2010 = [for i in wb.Countries -> i.Indicators.``Surface area (sq. km)``.[2010]]
R.summary(surface2010) |> R.print
//surface|>List.map log|>R.hist
surface2010|>R.log|>R.hist
R.plot(surface2000, pop2000)
R.plot(surface2010, pop2010)
//let csQuestions = """https://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=C#"""
//Questions.Load(csQuestions).Items|>Seq.iter (fun q-> printfn "%s" q.Title)

// Meging various data sources into single frame
let pollution = [for c in wb.Countries -> c.Indicators.``CO2 emissions (kt)``.[2000]]
let education = [for c in wb.Countries -> c.Indicators.``School enrollment, secondary (gross), gender parity index (GPI)``.[2000]]
R.data_frame
let rdf = 
          ["Pop2000", box pop2000
           "Pop2010", box pop2010
           "Surface2000", box surface2000
           "Surface2010", box surface2010
           "Pollution", box pollution
           "Education", box education]
           |> namedParams
           |> R.data_frame

//rdf.Class|>Array.iter (fun x->printfn "%s" x)           
//rdf|>R.plot
//rdf|>R.summary|>R.print

//deedle

let population2000 = series [for c in wb.Countries->c.Code, c.Indicators.``Population, total``.[2000]]
let population2010 = series [for c in wb.Countries->c.Code, c.Indicators.``Population, total``.[2010]]
let surface = series [for c in wb.Countries->c.Code, c.Indicators.``Surface area (sq. km)``.[2010]]

let ddf = frame ["Pop2000", population2000
                 "Pop2010", population2010
                 "Surface", surface]


ddf?Code <- ddf.RowKeys
let map = R.joinCountryData2Map(ddf, "ISO3", "Code")
R.mapCountryData(map, "Pop2000")

ddf?Density <- ddf?Pop2010/ddf?Surface
let map2 = R.joinCountryData2Map(ddf, "ISO3", "Code")
R.mapCountryData(map2, "Density")

ddf?Growth <- (ddf?Pop2010-ddf?Pop2000)/ddf?Pop2000
let map3 = R.joinCountryData2Map(ddf, "ISO3", "Code")
R.mapCountryData(map3, "Growth")
//open RProvider.utils
//R.install_packages(["rworldmap"])