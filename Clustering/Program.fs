// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO
open FSharp.Data
open FSharp.Charting
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
//
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
open MathNet.Numerics
open MathNet.Numerics.Providers.LinearAlgebra.Mkl
// move calculations to gpu
Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

let folder = __SOURCE_DIRECTORY__
let file = "userprofiles-toptags.txt"
let headers, observations = 
    let raw = folder + "/"+file|>File.ReadAllLines
    let headers = (raw.[0].Split ',').[1..]
    let observations = raw.[1..]|>Array.map (fun line-> (line.Split ',').[1..])|>Array.map (Array.map float)
    headers, observations

type Observation = float[]
let features = headers.Length

let rowNormalizer(obs:Observation):Observation = 
    let max = obs|>Seq.max
    obs|> Array.map (fun tagUse -> tagUse/max)

let observations1 = observations
                    |>Array.map (Array.map float)
                    |>Array.filter (fun x->Array.sum x > 0.)
                    |>Array.map rowNormalizer

let labels = ChartTypes.LabelStyle(Interval=0.25)


let pickFrom size k = 
    let rng = System.Random()
    let rec pick(set:int Set) = 
        let candidate = rng.Next(size)
        let set = set.Add candidate
        if set.Count = k then set
        else pick set
    pick Set.empty |>Seq.toArray

let initialize observations k = 
    let size = observations|>Array.length
    let centroids = pickFrom size k|> Array.mapi (fun i index -> i+1, observations.[index])
    let assignments = observations|>Array.map (fun x-> 0, x)
    (assignments, centroids)

let clusterize distance centroidOf observations k = 
    let rec search(assignments, centroids) = 
        let classifier observation = centroids|> Array.minBy (fun (_,centroid) -> distance observation centroid)|>fst
        let assignments' = assignments
                            |> Array.map (fun (_,observation)->
                            let closestCendtroidId = classifier observation
                            (closestCendtroidId, observation))
        let changed = (assignments, assignments')
                      ||>Seq.zip|>Seq.exists(fun((oldClusterId,_),(newClusterId,_))->not(oldClusterId=newClusterId))
        if changed 
        then 
             let centroids' = assignments'
                              |>Seq.groupBy fst
                              |>Seq.map(fun (clusterID, group) -> clusterID, group|> Seq.map snd|>centroidOf)|>Seq.toArray
             search (assignments', centroids')
        else centroids, classifier
    let initialValues = initialize observations k
    search initialValues

let distance(obs1:Observation)(obs2:Observation) = 
    (obs1,obs2)
    ||>Seq.map2 (fun u1 u2 -> pown (u1-u2) 2)
    |>Seq.sum

let squareError(obs1:Observation)(obs2:Observation) = 
    distance obs1 obs2

let centroidOf(cluster:Observation seq) = 
    Array.init features (fun f -> cluster|> Seq.averageBy(fun user -> user.[f]))

let ruleOfThumb(n:int) = sqrt(float n/2.)
let k_ruleOfThumb = ruleOfThumb(observations1.Length)
let RSS(dataset:Observation[]) centroids = 
    dataset|> Seq.sumBy(fun obs -> centroids|> Seq.map (squareError obs)|> Seq.min)
let AIC (dataset:Observation[]) centroids = 
    let k = centroids|>Seq.length
    let m = dataset.[0]|>Seq.length
    RSS dataset centroids + float (2*m*k)

let performClusterization() = 
    let (clusters, classifier) = 
         let clustering = clusterize distance centroidOf
         let k = 5
         clustering observations1 k
    clusters|>Seq.iter (fun(id, clusterInfo) ->
                            printfn "CLUSTER %i" id
                            clusterInfo
                            |>Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

    observations1|> Seq.countBy (fun obs->classifier obs)|>Seq.iter (fun (clusterId, count)->printfn "ClusterId - %i. Count - %i" clusterId count)
    ignore(Console.ReadLine())
    let chart = (Chart.Combine
                   [
                    for (id, clusterInfo) in clusters ->
                         clusterInfo
                         |> Seq.mapi (fun i value -> headers.[i], value)
                         |> Chart.Bar
                   ]
                 |> fun chart->chart.WithXAxis(LabelStyle=labels)).ShowChart()
    System.Windows.Forms.Application.Run(chart)
    0

let checkBestCluster() = 
  let line = 
     [1..25]
      |> Seq.map (fun k ->
         let value =
           [ for _ in 1 .. 10 ->
             let (clusters, classifier) =
                let clustering = clusterize distance centroidOf
                clustering observations1 k
             AIC observations1 (clusters |> Seq.map snd) ]
           |> List.average
         k, value)
      |> Chart.Line
  let chart = line.ShowChart()
  System.Windows.Forms.Application.Run(chart)


//let (bestClusters, bestClassifier) = 
//    let clustering = clusterize distance centroidOf
//    let k = 10
//    seq {
//      for _i in [1..20] ->
//         clustering observations1 k
//    }
//    |> Seq.minBy (fun (cs, f)-> RSS observations1 (cs|>Seq.map snd))
 
open MathNet.Numerics.Statistics
[<EntryPoint>]
let main argv =
    let feats = headers.Length
    let correlations =
     observations
     |> Matrix.Build.DenseOfColumnArrays
     |> Matrix.toRowArrays
     |> Correlation.PearsonMatrix
    let correlated =
        [
           for col in 0 .. (feats - 1) do
           for row in (col + 1) .. (feats - 1) ->
                correlations.[col,row], headers.[col], headers.[row]
        ]
        |> Seq.sortBy (fun (corr, f1, f2) -> - abs corr)
        |> Seq.take 20
        |> Seq.iter (fun (corr, f1, f2) ->
               printfn "%s %s : %.2f" f1 f2 corr)
    ignore(Console.ReadLine())
//    bestClusters|> Seq.iter (fun (id,profile) ->
//            printfn "CLUSTER %i" id
//            profile 
//            |> Array.iteri (fun i value -> 
//               if value > 0.2 then printfn "%16s %.1f" headers.[i] value))
    ignore (Console.ReadLine())
    ignore (checkBestCluster())
    ignore (performClusterization())
    printfn "%16s %8s %8s %8s" "Tag Name" "Avg" "Min" "Max"
    headers|> Array.iteri (fun i name->
              let col = observations|>Array.map (fun obs->obs.[i])
              let avg = col|>Array.average
              let min = col|>Array.min
              let max = col|>Array.max
              printfn "%16s %8.1f %8.1f %8.1f" name avg min max)

    let chart = (headers|> Seq.mapi(fun i name ->
                        name,
                        observations|>Seq.averageBy (fun obs->obs.[i]))
                     |> Chart.Bar
                     |> fun chart->chart.WithXAxis(LabelStyle =labels)).ShowChart()
    System.Windows.Forms.Application.Run(chart)
    ignore (Console.ReadLine())
    0 