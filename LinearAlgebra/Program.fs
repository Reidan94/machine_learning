// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open FSharp.Data
open FSharp.Charting
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type Data = CsvProvider<"day.csv">
let dataset = Data.Load("day.csv")
let data = dataset.Rows

type Obs = Data.Row
type Model = Obs -> float
type Featurizer = Obs -> float list

type Vec = Vector<float>
type Mat = Matrix<float>

let seed = 314159
let rng = System.Random seed

// Fisher-Yates shuffle
let shuffle(arr:'a []) = 
    let arr = Array.copy arr
    let l = arr.Length
    for i in (l-1).. -1 .. 1 do
       let temp = arr.[i]
       let j = rng.Next(0,i+1)
       arr.[i] <- arr.[j]
       arr.[j] <- temp
    arr

let exampleFeaturizer (obs:Obs) = 
     [1.0; float obs.Instant]

let cost(theta:Vec)(Y:Vec)(X:Mat) =  
    let ps = Y - theta * X.Transpose()
    ps * ps |> sqrt

let predict (theta:Vec)(v:Vec) = 
    theta * v 

let predictor (f:Featurizer) (theta:Vec) = 
    f>>vector>>((*) theta)

let estimate(Y:Vec)(X:Mat) = 
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

let evaulate (model:Model) (data: Obs seq) = 
    data
    |> Seq.averageBy (fun obs -> (model obs) - float obs.Cnt)

let model(f:Featurizer)(data:Obs seq) = 
    let Yt, Xt = data|> Seq.toList|>List.map (fun obs-> float obs.Cnt, f obs)|>List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta
    theta, predict  

[<EntryPoint>]
let main argv =     
    let X = matrix [for obs in data -> [1.; float obs.Instant]]
    let Y = vector [for obs in data ->  float obs.Cnt]
    let theta = vector [4.;6.5]
    let training, validation = 
        let shuffled = data|>Seq.toArray|>shuffle
        let size = int (0.7 * (float shuffled.Length))
        shuffled.[..size],
        shuffled.[size+1..]

    let (theta0, model0) = model exampleFeaturizer training
    evaulate model0 training |> printfn "Training set evaulation - %.4f"
    evaulate model0 validation |> printfn "Validation set evaulation - %.4f"

    let chart =  (Chart.Combine [
                     Chart.Line [for obs in data -> float obs.Cnt]
                     Chart.Line [for obs in data -> model0 obs] 
                 ]).ShowChart()
    System.Windows.Forms.Application.Run(chart)
    ignore (Console.ReadLine())
    printfn "%.3f" (predict theta (X.Row(0)))
    ignore (Console.ReadLine())
    let ps = theta * theta
    printfn "%.3f" ps
//    printfn "C - %.3f" C
    ignore (Console.ReadLine())
//    for i in C do
//        printfn "%3.f" i

    printfn "%A" argv
    0 // return an integer exit code
