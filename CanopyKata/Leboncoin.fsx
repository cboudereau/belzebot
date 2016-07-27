#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""

open System

module canopy = 
    open canopy
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

module SearchDomain =
    type Title = Title of string
    type City = City of string    
    type ZipCode = ZipCode of string
    type Bid = 
        { Title:Title
          ZipCode:ZipCode
          City:City
          Date:string
          Url:Uri }

    type PageCount = PageCount of int

    type SearchResult = 
        { Bids:Bid list
          PageCount: PageCount }
    type Author = Author of string
    type Price = Price of decimal
    type Category = Category of string

    type DetailedBid = 
        { Author:Author
          Title:Title
          Date:string
          Price:Price
          ZipCode:ZipCode
          City:City
          Category:Category
          Url:Uri
          Datas : Map<string, string> }

open canopy
open runner

module Parser = 
    open SearchDomain
    let price data = 
        match data with
        | Regex @"([0-9\s]*)" [p] -> p.Replace(" ", "") |> decimal |> Price
        | _ -> failwith "enable to find price"

module SeLoger = 
    open SearchDomain

    let search category zipCode city : SearchResult = failwith ""

    let detail (uri:Uri) : DetailedBid = 
        uri |> string |> url

//        element ".data"

        failwith ""

module Leboncoin = 
    open SearchDomain

    let private location datas = 
        match datas |> Map.find "Ville" with
        | Regex @"([^0-9]*)\s*([0-9]*)" [city; zipCode] -> (City city, ZipCode zipCode)
        | l -> failwithf "failed to find city and zipcode %s" l

    let search (Category category) zipCode city = 

        url "https://www.leboncoin.fr/annonces/offres/ile_de_france/"
        element "span.searchbar.toggleElement.button-white-mobile" |> click

        let selectCategory cat = 
            elements "#search_category option" 
            |> Seq.filter(fun e -> e.Text.Contains(cat) )
            |> Seq.head
            |> click

        let selectLocation (ZipCode zipCode) (City city) = 
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
            { Bid.Title = e |> elementWithin "h2" |> read |> Title
              ZipCode = zipCode
              City = city
              Date = e |> elementsWithin ".item_supp" |> Seq.last |> read
              Url = Uri(e |> link) }

        { Bids = elements "section.tabsContent li" |> Seq.map toBid |> Seq.toList
          PageCount = 
            match someElement "a#last" |> Option.map(href >> Uri >> queryParameters >> Seq.filter(fst >> (=) "o") >> Seq.exactlyOne >> snd >> int) with
            | Some pageCount -> PageCount pageCount
            | None -> PageCount 1 }

    let detail (uri:Uri) = 
        uri |> string |> url
        let datas = 
            elements "h2.clearfix"
            |> Seq.map(fun e -> e |> elementWithin "span.property" |> read, e|> elementWithin "span.value" |> read)
            |> Map.ofSeq
        let (city, zipcode) = location datas
        { Author=element ".title" |> read |> Author
          Title = element "header > h1" |> read |> Title
          Category = elements "#main > section > nav > ul li a" |> Seq.last |> read |> Category
          Date = element "section.properties.lineNegative > p" |> read
          Url = uri
          ZipCode = zipcode
          City = city
          Datas = datas
          Price = datas |> Map.find "Prix" |> Parser.price }

module Analyzer = 
    open SearchDomain
    let bidDistance x y = 
        let keys = Map.toList >> List.map fst
        let keys = x.Datas |> keys |> Seq.append (y.Datas |> keys) |> Set.ofSeq 

        let (Title titlex) = x.Title
        let (Title titley) = y.Title

        let (Author authorx) = x.Author
        let (Author authory) = y.Author

        let titleL = levenshtein titlex titley
        let authorL = levenshtein authorx authory

        let dataL = 
            keys
            |> Set.map(fun k ->
                match x.Datas |> Map.tryFind k, y.Datas |> Map.tryFind k with
                | Some xv, Some yv -> levenshtein xv yv
                | Some v, None | None, Some v -> levenshtein "" v
                | None, None -> 0)
            |> Seq.sum
        
        titleL + authorL + dataL

    let findDuplicates search detail pruner bid = 
        let sr = search bid.Category bid.ZipCode bid.City
        sr.Bids 
        |> List.map(fun b -> b.Url)
        |> List.filter((<>) bid.Url)
        |> List.map(fun uri -> detail uri)
        |> List.map(fun x -> x, bidDistance (pruner bid) (pruner x))
        |> List.filter(fun (_, d) -> d <= 10)
        |> List.map fst

module Pruner = 
    open SearchDomain
    let leboncoinHouseBid b = 
        let blacklist = [ "Référence" ]
        let whitelisted k _ = blacklist |> List.exists((=)k) |> not
        { b with 
            Author = Author ""
            Datas = b.Datas |> Map.filter whitelisted }


fsi.AddPrinter<Uri>(fun u -> u.ToString())

open canopy
open runner

start chrome
pin Right
open SearchDomain

let searchResult = Leboncoin.search (Category "Ventes immobilières") (ZipCode "77210") (City "Samoreau")
//let searchResult = search "Voitures" "77210" "Samoreau" 

searchResult.Bids
|> Seq.map(fun b -> b.Url |> Leboncoin.detail)
|> Seq.head

let x = 
    "https://www.leboncoin.fr/ventes_immobilieres/917447320.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail

let y = 
    "https://www.leboncoin.fr/ventes_immobilieres/913646798.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail

let duplicates = Analyzer.findDuplicates Leboncoin.search Leboncoin.detail Pruner.leboncoinHouseBid x

quit ()
