// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open FSharp.Data
open System
type Titanic = CsvProvider<"titanic.csv">

let dataset = Titanic.GetSample()
type Passanger = Titanic.Row

let survivalRate(passengers:Passanger seq)=
    let total = passengers|>Seq.length
    let survived = passengers|> Seq.filter(fun p-> p.Survived)|> Seq.length
    100.0*(float survived)/(float total) 

let mostFrequentLabeledIn group =
    group
    |> Seq.countBy snd
    |> Seq.maxBy snd
    |> fst

let learn sample extractFeature extractLabel = 
    let groups =
        sample
        |> Seq.map(fun obs-> extractFeature obs, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat, group)-> feat, mostFrequentLabeledIn group)

    let classifier obs = 
        let featuredValue = extractFeature obs
        groups
        |> Seq.find (fun (f,_) -> f = featuredValue)
        |> snd
    classifier

let hasData extractFeature = extractFeature >> Option.isSome
let betterLearn sample extractFeature extractLabel = 
    let branches = 
        sample
        |> Seq.filter (extractFeature |> hasData)
        |> Seq.map (fun obs -> extractFeature obs |> Option.get, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat, group) -> feat, mostFrequentLabeledIn group)
        |> Map.ofSeq

    let labelForMissingValues = 
        sample
        |> Seq.countBy extractLabel
        |> Seq.maxBy snd
        |> fst

    let classifier obs =
        let featuredValue = extractFeature obs
        match featuredValue with
        | None -> labelForMissingValues
        | Some(value) -> 
          match(branches.TryFind value) with
          | None -> labelForMissingValues
          | Some(predictedLabel) -> predictedLabel
    classifier


let survived (p:Passanger) = p.Survived
let sex (p:Passanger) = p.Sex
let pClass (p:Passanger) = p.Pclass

let sexClassifier = survived |> learn(dataset.Rows) sex
let classClassifier = survived |> learn(dataset.Rows) pClass

[<EntryPoint>]
let main argv = 
    dataset.Rows
    |> Seq.averageBy (fun p -> if p.Survived = classClassifier p then 1.0 else 0.0)
    |> printfn "Classification based on sex - %A"
    ignore (Console.ReadLine())
    dataset.Rows
    |> Seq.countBy (fun passenger -> passenger.Survived)
    |> Seq.iter (printfn "%A")

    dataset.Rows
    |> Seq.averageBy (fun passenger -> if passenger.Survived then 1.0 else 0.0)
    |> (printfn "%A")

    printfn "By sex"
    let bySex = dataset.Rows|>Seq.groupBy(fun p->p.Sex)|>Seq.iter(fun (s,g)->printfn "%A" (survivalRate g))
    printfn "By pclass"
    let byClass = dataset.Rows|>Seq.groupBy(fun p->p.Pclass)|>Seq.iter(fun (s,g)->printfn "%A %A" s (survivalRate g))    
    ignore (Console.ReadLine())
    printfn "%A" argv
    0 // return an integer exit code
