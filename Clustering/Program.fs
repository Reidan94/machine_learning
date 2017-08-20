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
    let random = System.Random()
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

let covarianceMatrix(M:Matrix<float>) = 
    let cols = M.ColumnCount
    let C = DenseMatrix.create cols cols Matrix.Zero
    for c1 in 0 .. (cols-1) do
        C.[c1,c1] <- Statistics.Statistics.Variance (M.Column c1)
        for c2 in (c1+1)..(cols-1) do
            let cov = Statistics.Statistics.Covariance(M.Column c1, M.Column c2)
            C.[c1,c2] <- cov
            C.[c2,c1] <- cov
    C

let normalize dim (observation:float[][]) = 
    let averages = 
        Array.init dim (fun i ->  
              observations
              |> Seq.averageBy (fun x->x.[i]))

    let stdDevs = 
        Array.init dim (fun i -> 
              let avg = averages.[i]
              observations
              |> Seq.averageBy (fun x ->
                  pown (float x.[i] - avg) 2 |> sqrt))

    observations
    |> Array.map (fun row ->
           row
           |> Array.mapi (fun i x ->
                    (float x - averages.[i])/stdDevs.[i]))

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

//let( bestClusters, bestClassifier) = 
//    let clustering = clusterize distance centroidOf
//    let k = 10
//    seq {
//      for _i in [1..5] ->
//         clustering observations1 k
//    }
//    |> Seq.minBy (fun (cs, f)-> RSS observations1 (cs|>Seq.map snd))
 
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra

let pca (observations:float[][]) =
    let factorization = 
        observations
        |> Matrix.Build.DenseOfRowArrays
        |> covarianceMatrix
        |> Matrix.eigen
     
    let eigenValues = factorization.EigenValues
    let eigenVectors = factorization.EigenVectors

    let projector (obs:float[]) = 
        let obsVector = obs |> Vector.Build.DenseOfArray
        (eigenVectors.Transpose() * obsVector)
        |> Vector.toArray
    (eigenValues, eigenVectors), projector  
 
let scale(row:float[]) = 
    let min = row|>Array.min
    let max = row|>Array.max

    if min = max then row 
    else
        row|> Array.map(fun x -> (x - min) / (max-min))

let test = observations.[..99] |> Array.map scale
let train = observations.[100..] |> Array.map scale

let distance2 (row1:float[]) (row2:float[]) =
    (row1, row2)
    ||> Array.map2 (fun x y -> pown (x-y) 2)
    |> Array.sum

let similarity(row1:float[]) (row2:float[]) =
    1./(1. + distance row1 row2)

let split(row:float[]) = 
    row.[1..19],row.[20..]

let weights(values:float[]) = 
    let total = values|> Array.sum
    values|> Array.map(fun x->x/total)

let predict(row:float[]) = 
    let known, unknown = row |> split
    let similarities = 
        train
        |> Array.map (fun example ->
            let common , _ = example |> split
            similarity known common)|> weights

    [| for i in 20..29 -> 
           let column = train|> Array.map (fun x -> x.[i])
           let prediction = 
             (similarities, column) 
             ||> Array.map2 (fun s v -> s*v)
             |> Array.sum
           prediction |]

let validation() =  
    test
    |> Array.map (fun obs ->
        let actual = obs|>split|>snd
        let predicted = obs|> predict
        let recommended,observed = 
            Array.zip predicted actual
            |> Array.maxBy fst
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct cals %.3f"
    ()

[<EntryPoint>]
let main argv =
    let targetTags = headers.[20..]
    let rez = predict test.[0] |> Array.zip targetTags
    validation()
    ignore(Console.ReadLine())
    let normalized = normalize (headers.Length) observations
    let (eValues, eVectors), projector = pca normalized   
    let project = projector observations.[1]
    let total = eValues|> Seq.sumBy (fun x -> x.Magnitude)
    eValues
    |> Vector.toList
    |> List.rev
    |> List.scan (fun (percent, cumul) value ->
            let percent = 100. * value.Magnitude / total
            let cumul = cumul+percent
            (percent, cumul))(0.,0.)
    |> List.tail
    |> List.iteri (fun i (p,c) -> printfn "Feat %2i: %.2f%% (%.2f%%)" i p c)

    ignore (Console.ReadLine())
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