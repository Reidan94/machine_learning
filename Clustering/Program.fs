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
Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

let folder = __SOURCE_DIRECTORY__
let file = "userprofiles-toptags.txt"
let headers, observations = 
    let raw = folder + "/"+file|>File.ReadAllLines
    let headers = (raw.[0].Split ',').[1..]
    let observations = raw.[1..]|>Array.map (fun line-> (line.Split ',').[1..])|>Array.map (Array.map float)
    headers, observations

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

type Observation = float[]
let features = headers.Length

let distance(obs1:Observation)(obs2:Observation) = 
    (obs1,obs2)
    ||>Seq.map2 (fun u1 u2 -> pown (u1-u2) 2)
    |>Seq.sum

let centroidOf(cluster:Observation seq) = 
    Array.init features (fun f -> cluster|> Seq.averageBy(fun user -> user.[f]))

let performClusterization() = 
    let observations1 = observations|>Array.map (Array.map float)|>Array.filter (fun x->Array.sum x > 0.)
    let (clusters, classifier) = 
         let clustering = clusterize distance centroidOf
         let k = 5
         clustering observations1 k
    clusters|>Seq.iter (fun(id, clusterInfo) ->
                            printfn "CLUSTER %i" id
                            clusterInfo
                            |>Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

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

[<EntryPoint>]
let main argv = 
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