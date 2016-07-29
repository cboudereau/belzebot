#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""
#r """..\packages\FSharp.Data\lib\net40\FSharp.Data.dll"""

type City = City of string    
type ZipCode = ZipCode of string

type Depth = Depth of int

open System

module String = 
    let icontains (search:string) (source:string) = 
        match search, source with
        | null, null -> true
        | _, null | null, _ -> false
        | _ -> source.IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1

    let (|Contains|_|) search source = 
        if icontains search source then Some source
        else None

module DataBank = 
    open FSharp.Data

    type private Cities = CsvProvider< """villes_france_sample.csv""", IgnoreErrors=true >

    let private rows = Cities.Load("""villes_france.csv""")
    let private datas = rows.Rows |> Seq.map(fun i -> ZipCode i.``Code postal``, City i.``Nom reel``) |> Seq.toList
    let ZipCodeIndex =  datas |> Map.ofList
    let CityIndex = datas |> Seq.map(fun (x, y) -> y, x) |> Map.ofSeq

    let tryFindCity candidate = 
        datas 
        |> List.filter(fun (zc, City c) -> String.icontains candidate c)
        |> List.tryHead

    let (|CityData|_|) = tryFindCity

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

module SearchDomain =
    type Title = Title of string
    type Price = Price of decimal
    
    type Range<'t> = { Min:'t; Max:'t }

    type Category = 
        | PropertyCategory 

    type PageCount = PageCount of int

    type Bid = 
        { ZipCode:ZipCode
          City:City
          Category:Category
          Price:Price
          Url:Uri }

    type SearchResult = 
        { Bids:Bid list
          PageCount: PageCount }

    type Author = Author of string
    type DetailedBid = 
        { Bid:Bid
          Author:Author
          Title:Title
          Date:DateTime
          Datas : Map<string, string> }

open canopy
open runner

module Parser = 
    open SearchDomain
    
    let price data = 
        match data with
        | Regex @"[^0-9]*([0-9\s]+)" [p] -> p.Replace(" ", "") |> decimal |> Price
        | _ -> failwith "enable to find price"
    
    let zipCode = function
        | Regex @"([0-9]{5})" [zp] -> zp |> ZipCode
        | _ -> failwith "failed to find zip code"

    let (|PriceData|_|) data = 
        match data with
        | Regex @"([0-9\s]{2,})" [p] -> p.Replace(" ", "") |> decimal |> Price |> Some
        | _ -> None

module DataKind = 
    let EnergyClass = "EnergyClass"
    let GES = "GES"
    let Surface = "Surface"
    let NumberOfRooms = "NumberOfRooms"

module Cache = 
    open System.Collections.Generic
    
    let cache now  =
        let datas = Dictionary<'a, (DateTime * 'b)>()
        let o = new Object ()
        fun timeout f x ->
            lock o <| fun () ->
                let now = now ()
                match datas.TryGetValue(x) with
                | true, (date, data) when date + timeout > now -> 
                    printfn "ok %O %A" date data
                    data
                | true, (date, data) -> 
                    printfn "timeout %O / %A" date data
                    let v = f x
                    datas.[x] <- (now, v)
                    v
                | _ -> 
                    printfn "missed %O / %A" now x
                    let v = f x
                    datas.[x] <- (now, v)
                    v

module SeLoger = 
    open SearchDomain

    let parseCity = function
        | Regex @"[^à].+à\s*(.*)" [city] 
        | Regex @"([^(]*)\s*" [city] -> city |> City
        | _ -> failwith "can't find seloger.com city from detail"
    
    let search (category, ZipCode zipCode, City city, rangeO) = 
        url "http://www.seloger.com/recherche-avancee.html?idtypebien=1,2"
        element "#ville_p" << city
        waitFor <| fun () -> element "#autoSuggestionsList_p" |> read |> String.icontains city
        elements "#autoSuggestionsList_p > ul > li"
        |> List.filter(fun e -> e |> read |> String.icontains city)
        |> List.head
        |> click

        match rangeO with 
        | Some range ->
            let (Price pmin) = range.Min
            let (Price pmax) = range.Max
            element """input[name="pxmin"]""" << (pmin |> int |> string)
            element """input[name="pxmax"]""" << (pmax |> int |> string)
        | None -> ()
        
        element "#ville_p" |> click

        press enter

        let pageCount = 
            match someElement "div.annonce__footer__pagination > p" |> Option.map read with
            | Some (Regex (@"[0-9]*\s*/\s*([0-9]*)") [p]) -> p |> int |> PageCount
            | _ -> PageCount 1

        let bids = 
            let (PageCount pc) = pageCount
            let rec bids i = 
                seq {
                    yield! 
                        elements ".liste_resultat > article"
                        |> List.map(fun e ->
                            { ZipCode = ZipCode zipCode
                              City = City city
                              Category = category
                              Url = e |> link |> Uri
                              Price = e |> elementWithin "div > a.amount" |> read |> Parser.price } ) 
                    if i < pc then
                        let next = i + 1
                        currentUrl () |> Uri |> setParameter "LISTING-LISTpg" (string next) |> string |> url
                        yield! bids next }
            bids 1
        
        { PageCount = pageCount
          Bids = bids |> Seq.toList }

    let detail (uri:Uri) : DetailedBid = 
        uri |> string |> url
        
        let (zipCode, city) = 
            match uri.PathAndQuery.Split('/').[4] with
            | Regex @"([^-0-9]*)" [DataBank.CityData city] -> city
            | s -> failwithf "failed to parse city %s" s
                
        { Bid = 
            { Url = uri
              ZipCode = zipCode
              City = city
              Category = Category.PropertyCategory
              Price = 
                match someElement "#price" with 
                | Some e -> e
                | None -> element ".price"
                |> read |> Parser.price }
          Author = "" |> Author
          Title = 
            match someElement "h1.detail-title" |> Option.map (read >> Title) with
            | Some t -> t
            | None -> Title ""
          Date = DateTime.UtcNow
          Datas =     
            try
                elements "#detail > ol > li"
                |> List.map read
                |> List.map(fun x -> x, x)
                |> Map.ofList
            with 
            | :? CanopyElementNotFoundException -> Map.empty }

module Leboncoin = 
    open SearchDomain
    open Parser

    let private location datas = 
        match datas |> Map.find "Ville" with
        | Regex @"([^0-9]*)\s*([0-9]*)" [city; zipCode] -> (City (city.Trim()), ZipCode zipCode)
        | l -> failwithf "failed to find city and zipcode %s" l

    let search (category, zipCode, city, rangeO) = 
        
        let cat = 
            match category with
            | PropertyCategory -> "Ventes immobilières"

        url "https://www.leboncoin.fr/annonces/offres/"
        element "span.searchbar.toggleElement.button-white-mobile" |> click

        let selectCategory cat = 
            elements "#search_category option" 
            |> List.filter(fun e -> e.Text.Contains(cat) )
            |> List.head
            |> click

        let selectLocation (ZipCode zipCode) (City city) = 
            element "input.nude" << zipCode
            waitFor <| fun () -> element ".location-list" |> read <> ""
            elements ".location-list li"
            |> List.filter(fun e -> e.Text.Contains(city))
            |> List.head
            |> click

        let selectRange = 
            let selectPriceOption v values = 
                values |> List.skipWhile(fst >> (>) v) |> List.head |> snd |> click            

            let pricesOptions selector = 
                elements selector
                |> List.collect(fun e -> 
                    match e |> read with
                    | PriceData p -> [ p, e ]
                    | _ -> [])

            function
            | Some range -> 
                pricesOptions "#ps > option" |> selectPriceOption range.Min
                pricesOptions "#pe > option" |> selectPriceOption range.Max
            | None -> ()

        selectCategory cat
        selectRange rangeO
        selectLocation zipCode city
        press enter

        let toBid e = 
            { Bid.ZipCode = zipCode
              City = city
              Category = category
              Url = Uri(e |> link)
              Price = e |> elementWithin "a > section > h3" |> read |> Parser.price }
        
        let pageCount = 
            match someElement "a#last" |> Option.map(href >> Uri >> queryParameters >> List.filter(fst >> (=) "o") >> List.exactlyOne >> snd >> int) with
            | Some pageCount -> PageCount pageCount
            | None -> PageCount 1

        let bids = 
            let (PageCount pc) = pageCount
            let rec bids i = 
                seq {
                    printfn "scanning lbc %i search page" i
                    yield! elements "section.tabsContent li" |> List.map toBid
                    if i < pc then 
                        let next = i + 1
                        currentUrl () |> Uri |> setParameter "o" (next |> string) |> string |> url
                        yield! bids next }
            
            bids 1

        { Bids = bids |> Seq.toList 
          PageCount = pageCount }

    let detail (uri:Uri) = 
        uri |> string |> url

        let datas = 
            elements "h2.clearfix"
            |> List.map(fun e -> e |> elementWithin "span.property" |> read, e|> elementWithin "span.value" |> read)
            |> Map.ofList
        
        let (city, zc) = location datas
        { Bid = 
            { Url=uri
              ZipCode = zc
              City = city
              Price = datas |> Map.find "Prix" |> Parser.price
              Category = 
                match elements "#main > section > nav > ul li a" |> Seq.last |> read with
                | "Ventes immobilières" -> Category.PropertyCategory
                | _ -> failwith "failed to get leboncoin category" }
          Author=element ".title" |> read |> Author
          Title = element "header > h1" |> read |> Title
          Date = DateTime.UtcNow
          Datas = datas }

module Crawler = 
    open SearchDomain

    let cached now timeout searches = searches |> List.map(fun (x, y) -> Cache.cache now timeout x, Cache.cache now timeout y)

    let crawl searches bid = 
        searches
        |> List.collect(fun (search, detail) ->
            let sr = 
                let range = 
                    let (Price p) = bid.Bid.Price
                    { Min=Price (p * 0.9m); Max=Price (p * 1.1m) }
                search (bid.Bid.Category, bid.Bid.ZipCode, bid.Bid.City, Some range)

            sr.Bids
            |> List.map(fun b -> b.Url)
            |> List.filter((<>) bid.Bid.Url)
            |> List.map(fun uri -> detail uri))

module Analyzer = 
    open SearchDomain
    let distance (x) (y) = 
        let keys = Map.toList >> List.map fst
        let keys = x.Datas |> keys |> List.append (y.Datas |> keys) |> Set.ofList

        let titleL = 
            let (Title titlex) = x.Title
            let (Title titley) = y.Title
            levenshtein titlex titley |> decimal

        let cityL = 
            let (City cityx) = x.Bid.City
            let (City cityy) = y.Bid.City
            levenshtein cityx cityy |> decimal

        let zipCodeL = 
            let (ZipCode zipCodex) = x.Bid.ZipCode
            let (ZipCode zipCodey) = y.Bid.ZipCode
            levenshtein zipCodex zipCodey |> decimal

        let (|Number|_|) v = 
            match Decimal.TryParse(v) with
            | true, d -> Some d
            | _ -> None

        let dataL = 
            keys
            |> Set.map(fun k ->
                match x.Datas |> Map.tryFind k, y.Datas |> Map.tryFind k with
                | Some (Number dx), Some (Number dy) -> 
                    let r = (abs (dx - dy) / dx)
                    printfn "%f %f -> %f" dx dy r
                    r
                | Some xv, Some yv -> levenshtein xv yv |> decimal
                | Some v, None | None, Some v -> levenshtein "" v |> decimal
                | None, None -> 0m)
            |> Seq.sum
        
        printfn "title: %f" titleL
        printfn "data: %f" dataL
        printfn "cityL: %f" cityL
        printfn "zipCodeL: %f" zipCodeL

        titleL + dataL + cityL + zipCodeL
    
    let distances pruner bid bids = bids |> List.map(fun x -> distance (pruner bid) (pruner x), x)
    
    let correlated scoredBids = scoredBids |> List.filter(fun (d, _) -> d < 1m)

    let approximated scoredBids = scoredBids |> List.filter(fun (d, _) -> d < 3.5m)

module Pruner = 
    open SearchDomain
    
    let no b = b
    
    module Property = 
        let private (|EnergyClassData|_|) (k,v) = 
            let (|Letter|_|) = function
                | Regex "([A-Z])\s" [c] -> Some c
                | _ -> None

            match k, v with
            | String.Contains "Classe énergie" _, Letter l -> Some (DataKind.EnergyClass, l)
            | _, Regex "DPE\s*\:\s*([A-Z])\s" [l] -> Some (DataKind.EnergyClass, l)
            | _ -> None

        let private (|GESData|_|) (k, v) = 
            let (|Letter|_|) = function
                | Regex "([A-Z])\s" [c] -> Some c
                | _ -> None

            match k, v with
            | _, Regex "GES\s*\:\s*([A-Z])\s" [l] -> Some (DataKind.GES, l)
            | String.Contains "GES" _, Letter l -> Some (DataKind.GES, l)
            | _ -> None

        let private (|NumberOfRoomsData|_|) (k,v) = 
            match k, v with
            | "Pièces", _ -> Some (DataKind.NumberOfRooms, v)
            | _, Regex "([0-9]+)\s*Pièces" [v] -> Some (DataKind.NumberOfRooms, v)
            | _ -> None
        
        let private (|SurfaceData|_|) (k,v) = 
            let (|Number|_|) = function
                | Regex "([0-9]{2,})" [surface] -> Some surface
                | _ -> None
            
            match k, v with
            | "Surface", Number n -> Some (DataKind.Surface, n)
            | _, Regex "Surface\s*de\s*([0-9]*)" [v] -> Some (DataKind.Surface, v)
            | _ -> None
        
        let prune b = 
            { b with 
                Author = Author ""
                Title = Title "" 
                Datas = 
                    b.Datas 
                    |> Map.toList
                    |> List.collect(fun x ->
                            match x with
                            | EnergyClassData r | NumberOfRoomsData r | SurfaceData r | GESData r -> [r]
                            | _ -> List.empty )
                    |> Map.ofList }

open SearchDomain

let crawl searches = 
    let now () = DateTime.UtcNow
    let timeout = (TimeSpan.FromHours(8.))
    
    let cachedSearches = searches |> Crawler.cached now timeout
    
    fun bid -> Crawler.crawl cachedSearches bid
        
        
fsi.AddPrinter<Uri>(fun u -> u.ToString())
fsi.AddPrinter<DateTime>(fun d -> d.ToString("O"))
open canopy
open runner

start chrome
pin Right
open SearchDomain

let searchResult = Leboncoin.search (PropertyCategory, ZipCode "77210", City "Samoreau", Some { Min=Price 300000m; Max= Price 380000m })
let searchResult2 = SeLoger.search (PropertyCategory, ZipCode "77210", City "Samoreau", Some { Min=Price 300000m; Max= Price 380000m })
//let searchResult = search "Voitures" "77210" "Samoreau" 

//searchResult.Bids
//|> Seq.map(fun b -> b.Url |> Leboncoin.detail)
//|> Seq.head

let w = 
    "http://www.seloger.com/annonces/achat/maison/samoreau-77/105946173.htm?ci=770442&idtt=2&idtypebien=1,13,14,2,9&LISTING-LISTpg=2"
    |> Uri
    |> SeLoger.detail

let x = 
    "https://www.leboncoin.fr/ventes_immobilieres/917447320.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail

let y = 
    "https://www.leboncoin.fr/ventes_immobilieres/913646798.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail

let z =
    "https://www.leboncoin.fr/ventes_immobilieres/988069608.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail

let c = 
    [ SeLoger.search, SeLoger.detail
      Leboncoin.search, Leboncoin.detail ]
    |> crawl

//screenshot @"D:\" "log"

//let bid = "https://www.leboncoin.fr/ventes_immobilieres/950107746.htm?ca=12_s" |> Uri |> Leboncoin.detail
let bid = "https://www.leboncoin.fr/ventes_immobilieres/985217137.htm?ca=12_s" |> Uri |> Leboncoin.detail

let r = c bid
let rp = r |> Analyzer.distances Pruner.Property.prune bid
rp |> List.map fst
rp |> Analyzer.correlated |> List.map(fun (s,b) -> s, b.Bid.Url) |> List.map snd

w.Datas |> Map.toList
Pruner.Property.prune w
Pruner.Property.prune x

let one   = ("https://www.leboncoin.fr/ventes_immobilieres/950107746.htm?ca=12_s" |> Uri |> Leboncoin.detail)
let other = ("http://www.seloger.com/annonces/achat/maison/samoreau-77/82469753.htm?ci=770442&idtt=2&idtypebien=1,2&org=advanced_search&pxmax=391600&pxmin=320400" |> Uri |> SeLoger.detail) 

Analyzer.distance 
    (Pruner.Property.prune one) 
    (Pruner.Property.prune other)

//SeLoger.search (Categories.Property, ZipCode "77210", City "Samoreau", None)

quit ()

Leboncoin.search (Category.PropertyCategory, ZipCode "77210", City "Avon", None)

let bagnole = 
    "https://www.leboncoin.fr/voitures/981613128.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail