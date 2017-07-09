open System.IO
open System

type Observation = {Label:string; Pixels:double[]}
type Distance = double[] * double[] -> double

let toObservation(csvData:string) = 
    let splitted = csvData.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
    let label = splitted.[0]
    let pixels = splitted.[1..] |> Array.map double
    {Label = label; Pixels = pixels }

let reader path = 
    let data = File.ReadAllLines path
    data.[1..]|> Array.map toObservation

let manhattanDistance(pixels1, pixels2) = 
    Array.zip pixels1 pixels2
    |> Array.map (fun(x, y) -> abs(x - y))
    |> Array.sum

let euclideanDistance(pixels1, pixels2) = 
    Array.zip pixels1 pixels2
    |> Array.map (fun(x, y) -> pown (x - y) 2)
    |> Array.sum
    |> sqrt

let train (trainingData:Observation[]) (dist:Distance) = 
    let classify(pixels:double[]) =
        trainingData
        |> Array.minBy (fun x -> dist(x.Pixels, pixels))
        |> fun best -> best.Label
    classify

// Chose correct path
let trainingPath = __SOURCE_DIRECTORY__ + @"\trainingsample.csv"
let trainingData = reader trainingPath  
let classifierManhattan  = train trainingData manhattanDistance
let classifierEuclidean = train trainingData euclideanDistance

let validationPath = __SOURCE_DIRECTORY__ + @"\validationsample.csv"
let validationData = reader validationPath
validationData
|> Array.averageBy (fun x-> if classifierEuclidean x.Pixels = x.Label then 100. else 0.)
|> printfn "Correct: %.3f"
