open System
open System.Collections.Generic
open System.IO

type MessageType = 
     | Normal
     | Spam

let parseDocType(label:string) = 
    match label with
    | "ham" -> Normal
    | "spam" -> Spam
    | _ -> failwith "Unknown  label"

let parseLine(line:string) = 
    let splitted = line.Split('\t')
    let label = splitted.[0]|>parseDocType
    let message  = splitted.[1]
    (label, message)

let fileName = __SOURCE_DIRECTORY__ + @"\SMSSpamCollection"
let dataset = 
    File.ReadAllLines fileName
    |> Array.map parseLine

let spamWithFree = 
    dataset
    |> Array.filter (fun(docType,_)-> docType = Spam)
    |> Array.filter (fun(_,sms)->sms.Contains("FREE"))
    |> Array.length 

printfn "Spam with free %d" spamWithFree

let normalWithFree =
    dataset
    |> Array.filter (fun (docType, _)->docType = Normal)
    |> Array.filter (fun (_, sms)->sms.Contains("FREE"))
    |> Array.length

printfn "Normal with free %d" normalWithFree

Console.WriteLine()