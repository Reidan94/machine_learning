// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
open System
open FSharp.Data
open RProvider
open RProvider.``base``
open RProvider.graphics
open RProvider.rworldmap
open Deedle
open FSharp.Charting

type Data = CsvProvider<"day.csv">
type Obs = Data.Row
type Model = Obs -> float

let dataset = Data.Load("day.csv")
let data2 = dataset.Rows

let model (theta0, theta1) (obs:Obs) = 
    theta0 + theta1 * (float obs.Instant)

let cost (obs:Obs seq)(model:Model) = 
     obs
     |> Seq.sumBy (fun o -> pown (float o.Cnt - model o) 2)
     |> sqrt

let overallCost = cost data2

let update alpha (theta0, theta1)(obs:Obs) =
    let y = float obs.Cnt
    let x = float obs.Instant
    let theta0' = theta0 - 2.0 * alpha * 1.0 * (theta0 + theta1 * x - y)
    let theta1' = theta1 - 2.0 * alpha * x *   (theta0 + theta1 * x - y)
    theta0', theta1'

let updateV2 alpha (theta0, theta1)(data:Obs seq) = 
    let updates = data |> Seq.map (update alpha (theta0, theta1))
    let theta0' = updates|> Seq.averageBy fst
    let theta1' = updates|> Seq.averageBy snd
    theta0',theta1'

let rec batch rate iters = 
    let rec search (t0, t1) i = 
        if i = 0 then (t0, t1)
        else 
            search (updateV2 rate (t0, t1) (data2)) (i-1)
    search (0.0, 0.0) iters

let stochastic rate (theta0, theta1) = 
    data2 |> Seq.fold (fun (t0,t1) obs ->update rate (t0,t1) obs) (theta0, theta1)

let movingAverage n (series:float seq) =
    series
    |> Seq.windowed n
    |> Seq.map (fun xs -> xs|>Seq.average)
    |> Seq.toList
    
let batched_error rate = 
    Seq.unfold (fun (t0,t1) ->
                    let (t0',t1') = updateV2 rate (t0,t1) data2
                    let err = model(t0,t1)|>overallCost
                    Some(err, (t0', t1'))) (0.0, 0.0)
    |> Seq.take 100
    |> Seq.toList
    |> Chart.Line

// Define your library scripting code here
[<EntryPoint>]
let main argv = 
    let rate = pown 0.1 8
    let errorChart = (batched_error rate).ShowChart()
    System.Windows.Forms.Application.Run(errorChart)
    ignore (Console.ReadLine())

    let model2 = model (stochastic rate (0.,0.))
    let count = [for i in data2 -> float(i.Cnt)]
    let chart = (Chart.Combine[
                        Chart.Line count
                        Chart.Line [for i in data2 -> model2 i]
                    ]).ShowChart()
    System.Windows.Forms.Application.Run(chart)
    ignore (Console.ReadLine())
    let tune_rate = [for i in 1 .. 20 -> (pown 0.1 i), stochastic (pown 0.1 i) (0.,0.)|> model |>(cost data2)]
    for kvp in tune_rate do
        let key, value  =  kvp
        printfn "KEY - %.10f VALUE - %.10f" key value
    
    ignore (Console.ReadLine())
    let obs100 = data2 |> Seq.item 100
    let updated = update 0.00001 (0.,0.) obs100
    let numbers = [1;3;4;67;190]
    printfn "Aggregated - %i" (numbers|> Seq.fold (fun total x -> total + x) 0)
    ignore (Console.ReadLine())
    printfn "Cost wiwthout update - %.4f" (cost [obs100] (model (0.,0.)))
    printfn "Cost after update - %.4f" (cost [obs100] (model updated))
    ignore (Console.ReadLine())

    let baseline = 
        let avg = data2|>Seq.averageBy(fun x->float x.Cnt)
        data2|>Seq.averageBy(fun x-> abs (float (x.Cnt) - avg))
    let model0 = model (4504., 0.)
    let model1 = model (6000., -4.5)
    let cost0 = overallCost model0
    let cost1 = overallCost model1
    printfn "Model0 cost - %.3f" cost0
    printfn "Model1 cost - %.3f" cost1
    ignore (Console.ReadLine())
    printfn "%f" baseline
    ignore (Console.ReadLine())

    let modelTesting = (Chart.Combine[
                          Chart.Line count
                          Chart.Line [for obs in data2 -> model0 obs]
                          Chart.Line [for obs in data2 -> model1 obs]]).ShowChart()
    System.Windows.Forms.Application.Run(modelTesting)
    ignore (Console.ReadLine())
    0

    let all = (Chart.Combine[
                  Chart.Line count
                  Chart.Line (movingAverage 7 count)
                  Chart.Line (movingAverage 30 count)
                 ]).ShowChart()
    System.Windows.Forms.Application.Run(all)
    Console.WriteLine("FINISHED")
    ignore (Console.ReadLine())
    0

