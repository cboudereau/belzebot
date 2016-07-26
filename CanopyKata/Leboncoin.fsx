#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""

open canopy
open runner
open System
open OpenQA.Selenium
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


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

let link (element:IWebElement) = element |> elementWithin "a" |> href

let queryParameters (uri:Uri) = 
    uri.Query.Substring(1).Split('&') 
    |> Seq.collect(fun x -> 
        match x.Split('=') with
        | [|name;value|] -> Some (name, value)
        | _ -> None
        |> Option.toList)
    |> Seq.toList

type Bid = 
    { Title:string
      Price:string
      ZipCode:string
      City:string
      Date:string
      Url:Uri }

type PageCount = PageCount of int

type SearchResult = 
    { Bids:Bid list
      PageCount: PageCount }

type DetailedBid = 
    { Author:string
      Title:string
      Date:string
      Category:string
      Url:Uri
      Datas : Map<string, string> }

let search category zipCode city = 

    url "https://www.leboncoin.fr/annonces/offres/ile_de_france/"
    element "span.searchbar.toggleElement.button-white-mobile" |> click

    let selectCategory cat = 
        elements "#search_category option" 
        |> Seq.filter(fun e -> e.Text.Contains(cat) )
        |> Seq.head
        |> click

    let selectLocation zipCode city = 
        element "input.nude" << zipCode
        waitFor <| fun () -> element ".location-list" |> read <> ""
        elements ".location-list li"
        |> Seq.filter(fun e -> e.Text.Contains(city))
        |> Seq.head
        |> click

    selectCategory category
    selectLocation zipCode city
    press enter

    let toBid e = 
        { Title = e |> elementWithin "h2" |> read
          Price = e |> elementWithin "h3.item_price" |> read
          ZipCode = zipCode
          City = city
          Date = e |> elementsWithin ".item_supp" |> Seq.last |> read
          Url = Uri(e |> link) }

    { Bids = elements "section.tabsContent li" |> Seq.map toBid |> Seq.toList
      PageCount = 
        match someElement "a#last" |> Option.map(href >> Uri >> queryParameters >> Seq.filter(fst >> (=) "o") >> Seq.exactlyOne >> snd >> int) with
        | Some pageCount -> PageCount pageCount
        | None -> PageCount 1 }

let getDetailedBid (uri:Uri) = 
    uri |> string |> url
    { Author=element ".title" |> read
      Title = element "header > h1" |> read
      Category = elements "#main > section > nav > ul li a" |> Seq.last |> read
      Date = element "section.properties.lineNegative > p" |> read
      Url = uri
      Datas=
        elements "h2.clearfix"
        |> Seq.map(fun e -> e |> elementWithin "span.property" |> read, e|> elementWithin "span.value" |> read)
        |> Map.ofSeq }

let bidDistance x y = 
    let keys = Map.toList >> List.map fst
    let keys = x.Datas |> keys |> Seq.append (y.Datas |> keys) |> Set.ofSeq 

    let titleL = levenshtein x.Title y.Title
    let authorL = levenshtein x.Author y.Author

    let dataL = 
        keys
        |> Set.map(fun k ->
            match x.Datas |> Map.tryFind k, y.Datas |> Map.tryFind k with
            | Some xv, Some yv -> levenshtein xv yv
            | Some v, None | None, Some v -> levenshtein "" v
            | None, None -> 0)
        |> Seq.sum
    
    titleL + authorL + dataL

let tryFindBidLocation bid = 
    match bid.Datas |> Map.find "Ville" with
    | Regex @"([^0-9]*)\s*([0-9]*)" [city; zipCode] -> Some (city, zipCode)
    | _ -> None

let findDuplicates pruner bid = 
    bid
    |> tryFindBidLocation
    |> Option.map(fun (city, zipCode) -> search bid.Category zipCode city)
    |> Option.map(fun sr -> sr.Bids |> List.map(fun b -> b.Url))
    |> Option.toList
    |> List.concat
    |> List.filter((<>) bid.Url)
    |> List.map(fun uri -> getDetailedBid uri)
    |> List.map(fun x -> x, bidDistance (pruner bid) (pruner x))
    |> List.filter(fun (_, d) -> d <= 10)
    |> List.map fst

let homeBid b = 
    let blacklist = [ "Référence" ]
    let whitelisted k _ = blacklist |> List.exists((=)k) |> not
    { b with 
        Author = ""
        Datas = b.Datas |> Map.filter whitelisted }

start chrome
pin Right

let searchResult = search "Ventes immobilières" "77210" "Samoreau" 
//let searchResult = search "Voitures" "77210" "Samoreau" 

searchResult.Bids
|> Seq.map(fun b -> b.Url |> getDetailedBid)
|> Seq.head

let x = 
    "https://www.leboncoin.fr/ventes_immobilieres/917447320.htm?ca=12_s"
    |> Uri
    |> getDetailedBid

let y = 
    "https://www.leboncoin.fr/ventes_immobilieres/913646798.htm?ca=12_s"
    |> Uri
    |> getDetailedBid

bidDistance (homeBid x) (homeBid y)


findDuplicates homeBid x

let uri = element "a#last" |> href |> Uri

uri  |> queryParameters |> Seq.filter(fst >> (=) "o") |> Seq.exactlyOne |> snd |> int |> PageCount

quit ()
