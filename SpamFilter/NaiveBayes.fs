namespace NaiveBayes
open System.IO

module classifier = 
       type Token = string
       type Tokenaizer = string -> Token Set
       type TokenaizedDoc = Token Set
       type DocsGroup = {Proportion:float; TokenFreequencies:Map<Token,float>}
       type MessageType = 
            | Normal
            | Spam
       
       let vocabulary (tokenaizer:Tokenaizer) (corpus:string seq) = 
           corpus
           |> Seq.map tokenaizer
           |> Set.unionMany  

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

       let getDataset fileName = 
           File.ReadAllLines fileName
           |> Array.map parseLine

       let tokenScore (group:DocsGroup)(token:Token) = 
           if group.TokenFreequencies.ContainsKey token 
           then log group.TokenFreequencies.[token]
           else 0.0
       
       let score (document:TokenaizedDoc)(group:DocsGroup) = 
           let scoreToken = tokenScore group
           log group.Proportion + 
           (document |> Seq.sumBy scoreToken)

       let classify (groups:(_*DocsGroup)[])(tokenaizer:Tokenaizer)(txt:string) = 
            let doc = tokenaizer txt
            let scoreByDoc = score doc
            groups
            |> Array.maxBy (fun (label, group) -> score doc group)
            |> fst

       let proportion count total = float count / float total
       let laplace count total = float(count + 1) / float(total + 1)
       let countIn (group:TokenaizedDoc seq) (token:Token) = 
           group
           |> Seq.filter (Set.contains token)
           |> Seq.length

       let analyze (group:TokenaizedDoc seq)
                   (totalDocs:int)
                   (classificationTokens:Token Set) =
             let groupSize = group |> Seq.length
             let score token =
                   let count = countIn group token
                   laplace count groupSize

             let scoredTokens =
                 classificationTokens
                 |> Set.map (fun token -> token, score token)
                 |> Map.ofSeq

             let groupProportion = proportion groupSize totalDocs
             {
               Proportion = groupProportion
               TokenFreequencies = scoredTokens
             }

       let learn (docs:(_*string)[])
                 (tokenaizer:Tokenaizer)
                 (classificationTokens:Token Set) = 
           let total = docs.Length
           docs
           |> Array.map (fun (label, docs) -> label, tokenaizer docs)
           |> Seq.groupBy fst
           |> Seq.map (fun (label, group) -> label, group|> Seq.map snd)
           |> Seq.map (fun (label, group) -> label, analyze group total classificationTokens)
           |> Seq.toArray
           
       let train(docs:(_ *string)[])
                (tokenaizer:Tokenaizer)
                (classifactionTokens:Token Set) = 
           let groups = learn  docs tokenaizer classifactionTokens
           let classifier = classify groups tokenaizer 
           classifier

