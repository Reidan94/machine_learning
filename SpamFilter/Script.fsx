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

Console.WriteLine()