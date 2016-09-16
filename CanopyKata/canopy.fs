module canopy
open canopy

open OpenQA.Selenium
open System.Text.RegularExpressions
open System

let levenshtein word1 word2 =
    let preprocess = fun (str : string) -> str.ToLower().ToCharArray()
    let chars1, chars2 = preprocess word1, preprocess word2
    let m, n = chars1.Length, chars2.Length
    let table : int[,] = Array2D.zeroCreate (m + 1) (n + 1)
    for i in 0..m do
        for j in 0..n do
            match i, j with
            | i, 0 -> table.[i, j] <- i
            | 0, j -> table.[i, j] <- j
            | _, _ ->
                let delete = table.[i-1, j] + 1
                let insert = table.[i, j-1] + 1
                //cost of substitution is 2
                let substitute = 
                    if chars1.[i - 1] = chars2.[j - 1] 
                        then table.[i-1, j-1] //same character
                        else table.[i-1, j-1] + 2
                table.[i, j] <- List.min [delete; insert; substitute]
    table.[m, n]

let attribute name (element:IWebElement) = element.GetAttribute(name)

let href element = attribute "href" element

let htmlClass element = attribute "class" element

let link (element:IWebElement) = element |> elementWithin "a" |> href

let queryParameters (uri:Uri) = 
    uri.Query.Substring(1).Split('&') 
    |> Seq.collect(fun x -> 
        match x.Split('=') with
        | [|name;value|] -> Some (name, value)
        | _ -> None
        |> Option.toList)
    |> Seq.toList

let setParameter name value (uri:Uri) = 
    let anchor = sprintf "%s=" name
    match uri.ToString() with
    | u when uri.Query.Contains(anchor) ->
        let parameters = 
            uri.Query.Substring(1).Split('&')
            |> Seq.map(fun pv -> 
                if pv.Contains(anchor) then sprintf "%s%s" anchor value
                else pv)
        
        sprintf "%s?%s" (uri.ToString().Split('?').[0]) (String.Join("&", parameters |> Seq.toArray))
        |> Uri
    | u -> 
        if u.Contains("?") then sprintf "%s&%s=%s" u name value |> Uri
        else sprintf "%s?%s=%s" u name value |> Uri
