// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I @"..\packages\"
#r @"FSharp.Data.2.3.3\lib\net40\FSharp.Data.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.Runtime.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.dll"
#r @"Deedle.1.2.5\lib\net40\Deedle.dll"
#r @"R.NET.Community.1.6.5\lib\net40\RDotNet.dll"
#r @"Deedle.RPlugin.1.2.5\lib\net40\Deedle.RProvider.Plugin.dll"
#r @"FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
open System
open FSharp.Data
open RProvider
open RProvider.``base``
open RProvider.graphics
open RProvider.rworldmap
open Deedle
open FSharp.Charting

type Data = CsvProvider<"day.csv">
Console.WriteLine("Hello")
let dataset = Data.Load("day.csv")
let data2 = dataset.Rows
let all = Chart.Line [for i in data2 -> i.Cnt]