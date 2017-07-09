open System
open System.Collections.Generic
open System.IO
#load "NaiveBayes.fs"
open NaiveBayes.classifier


//sets
let intVal = 23
printfn "%d" intVal
let set1 = set [1;2;3;3;3;3;3;3]
set1
|> Set.iter (fun x->printfn "%d" x)
let set2 = set [2;3;4;12]
Set.add 1290 set1 
Set.difference set1 set2
Set.difference set2 set1
let intersection = Set.intersect set1 set2
let union = Set.union set1 set2
intersection
|> Set.iter (fun x-> printfn "%d" x)

//arrays lazy initialization

let list = [for i in 1..10->i]
printfn "%s" (list.GetType().Name)

let array = [|for i in 1..10->i|]
printfn "%s" (array.GetType().Name)

type tokenizer2 = string -> string[]
let stringSplitter (str:string) = 
    str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)                 

open System
let someFun (group:(int*string)[]) = 
    group
    |> Array.map(fun (x, str) -> x, str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
    |> Seq.groupBy fst

let temp = [(1, [(1,[|"4";"4";"78"|]);(1,[|"4";"4";"78"|])]);(1, [(2,[|"4";"4";"78"|])])]
let temp2 = 
    temp
    |> Seq.map (fun (x,y) -> x, y|> Seq.map snd)

           
let rez = someFun[|(1, "3 4 6");(2, "32 44 65");(1, "390 4670 690")|]
let m = fst (Seq.head rez)
let n = fst (Seq.head (snd (Seq.head rez)))
for i in rez do
    let e1 = fst i
    let e2 = snd i
    printfn "Global key %d" e1
    for j in e2 do
        let numb = fst j
        printfn "Middle key %d" numb
        let arr = snd j
        for s in arr do
            printfn "%s" s

// lazy init. looks like IEnumerable in .NET
let lazyArray = seq {for i in 1..10->i}
printfn "%s" (lazyArray.GetType().Name)
lazyArray
|> Seq.iter (fun x-> printfn "%d" x)
printfn "%s" (lazyArray.GetType().Name)

let seq2 = 
    lazyArray
    |> Seq.map (fun x-> 
               printfn "Mathcing %i" x 
               2*x)
let infiniteSeq = Seq.initInfinite (fun x -> if x%2 = 0 then 1 else -1)
let sum = infiniteSeq |> Seq.take 1000000 |> Seq.sum
printfn "sum %d" sum
let arr = [|2;3;4;5;12;8;100|]
let counts = 
    arr
    |> Seq.countBy (fun x -> x)
    |> Seq.toArray

counts
|> Seq.iter (fun(x,y)-> printfn "%d %d" x y) 

open System.IO
#load "NaiveBayes.fs"
open NaiveBayes.classifier
            
// entry point for spam analizer
open System.Text.RegularExpressions
let matchWords = Regex(@"\w+")
let tokenizer (text:string) = 
           text.ToLowerInvariant()
           |> matchWords.Matches
           |> Seq.cast<Match>
           |> Seq.map (fun m-> m.Value)
           |> Set.ofSeq

let casedTokenizer (text:string) = 
           text
           |> matchWords.Matches
           |> Seq.cast<Match>
           |> Seq.map (fun m -> m.Value)
           |> Set.ofSeq

tokenizer "sdsdsdsd 1290129012 lkjdldfj"
let trainingSetPath = __SOURCE_DIRECTORY__ + @"\SMSSpamCollection"
let dataset = getDataset trainingSetPath
let validation, training = dataset.[..999], dataset.[1000..]
let allTokens = 
    training 
    |> Seq.map snd
    |> vocabulary tokenizer

let txtClassifier = train training tokenizer allTokens
validation
     |> Array.map (fun (msgType, txt) -> msgType, txtClassifier txt)
     |> Array.averageBy (fun (realMsgType, predictedMsgType) -> if realMsgType = predictedMsgType then 100.0 else 0.0) 
     |> printfn "Correct: %.3f"

let evaulate (tokenizer:Tokenaizer) (tokens: Token Set) = 
    let classifier = train training tokenizer tokens
    validation
       |> Array.map (fun (msgType, txt) -> msgType, classifier txt)
       |> Array.averageBy (fun (realMsgType, predictedMsgType) -> if realMsgType = predictedMsgType then 100.0 else 0.0) 
       |> printfn "Correct: %.3f"

evaulate tokenizer allTokens
Console.WriteLine("Finished!")