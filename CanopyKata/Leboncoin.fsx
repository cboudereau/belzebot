﻿#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""
#r """..\packages\FSharp.Data\lib\net40\FSharp.Data.dll"""
#r """..\packages\Outatime\lib\net452\Outatime.dll"""

#load "String.fs"
#load "canopy.fs"
//Chearper.ly

open SetTheory
open System

type City = City of string    
type ZipCode = ZipCode of string

type Depth = Depth of int

open System


module DataBank = 
    open FSharp.Data

    type private Cities = CsvProvider< """villes_france_sample.csv""", IgnoreErrors=true >

    let private rows = Cities.Load("""villes_france.csv""")
    let private datas = rows.Rows |> Seq.map(fun i -> ZipCode i.``Code postal``, City i.Slug) |> Seq.toList
    let ZipCodeIndex =  datas |> Map.ofList
    let CityIndex = datas |> Seq.map(fun (x, y) -> y, x) |> Map.ofSeq

    let tryFindCity candidate = 
        datas 
        |> List.filter(fun (zc, City c) -> String.icontains candidate c)
        |> List.tryHead

    let (|CityData|_|) = tryFindCity

module SearchDomain =
    open SetTheory
    
    type Title = Title of string
    type Price = 
        | Price of decimal
        static member MaxValue = Price Decimal.MaxValue
        static member MinValue = Price Decimal.MinValue
        
    type Category = 
        | PropertyCategory 

    type Bid = 
        { ZipCode:ZipCode
          City:City
          Category:Category
          Price:Price
          Url:Uri }

    type SearchResult = 
        { Bids:Bid list }

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
        | String.Regex @"[^0-9]*([0-9\s]+)" [p] -> p.Replace(" ", "") |> decimal |> Price
        | _ -> failwith "enable to find price"
    
    let zipCode = function
        | String.Regex @"([0-9]{5})" [zp] -> zp |> ZipCode
        | _ -> failwith "failed to find zip code"

    let (|PriceData|_|) data = 
        match data with
        | String.Regex @"([0-9\s]{2,})" [p] -> p.Replace(" ", "") |> decimal |> Price |> Some
        | _ -> None

module DataKind = 
    let EnergyClass = "EnergyClass"
    let GES = "GES"
    let Surface = "Surface"
    let NumberOfRooms = "NumberOfRooms"

module Cache = 
    open System.Collections.Generic
    
    let intervalCache now = 
        let datas = Dictionary<'a, IntervalValuedSet<'i, (DateTime * 'b) option>>()
        let o = new Object ()

        fun timeout f x ->
            lock o <| fun () ->
                let now = now ()
                match datas.TryGetValue(x.Value) with
                | false, _ -> 
                    printfn "missed full interval"
                    let data = f x
                    let update = 
                        (x.Interval := (now, data.Value)) 
                        |> List.singleton 
                        |> SetTheory.build 
                    
                    datas.[x.Value] <- (update |> SetTheory.contiguous)
                    
                    update |> SetTheory.lift snd
                | true, data -> 
                    let update = 
                        data
                        |> SetTheory.clamp x.Interval
                        |> SetTheory.map(fun i ->
                            function
                            | Some (date, value) when date + timeout > now -> 
                                (date, value)
                            | Some (date, _) ->
                                printfn "timeout %O" date
                                let data = f (i := x.Value)
                                (now, data.Value)
                            | _ -> 
                                printfn "missed partial interval %A" i
                                let data = f (i := x.Value) 
                                (now, data.Value))
                        |> SetTheory.toList
                        |> SetTheory.build
                    
                    datas.[x.Value] <- 
                        update
                        |> SetTheory.contiguous
                        |> SetTheory.lift2 (fun xO yO -> 
                            match xO, yO with
                            | Some x, Some y -> Some y
                            | Some v, _ | _, Some v -> Some v
                            | None, None -> None) datas.[x.Value]
                        |> SetTheory.merge

                    update |> SetTheory.lift snd
                        
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

module Pap = 
    open SearchDomain
    open SetTheory

    let private parsePrice p = (p:string).Replace(".", "") |> Parser.price

    let search criteria = 
//        let criteria = Price 200000M => Price 250000M := (PropertyCategory, ZipCode "77210", City "Samoreau")
        
        let (category, ZipCode zipCode, City city) = criteria.Value
        let range = criteria.Interval
        let (Price pmin) = range.Start
        let (Price pmax) = range.End

        url "http://www.pap.fr/annonce/vente-immobiliere"
        
        element "#token-input-geo_objets_ids" << sprintf "%s %s" city zipCode
        waitFor <| fun () -> 
            let value = element ".token-input-dropdown" |> read 
            value |> String.icontains "recherche" |> not && value <> ""
        element ".token-input-dropdown" |> click
        element "#prix_min" << (pmin |> int |> string)
        element "#prix_max" << (pmax |> int |> string)
        press enter

        let bids =
            let rec bids () = 
                seq {
                    yield! 
                        elements ".search-results-item"
                        |> List.filter(fun e -> e |> htmlClass |> String.icontains "annonce" |> not)
                        |> List.map(fun e ->
                            { Category = category
                              ZipCode = ZipCode zipCode
                              City = City city
                              Price = e |> elementWithin ".price" |> read |> parsePrice
                              Url = Uri(e |> link) }) 
                    match someElement ".next" with
                    | Some next when next.Displayed ->
                        click next
                        yield! bids ()
                    | _ -> yield! Seq.empty }

            bids ()
        
        range := { Bids = bids |> Seq.toList }

    let detail (uri:Uri) : DetailedBid = 
        uri |> string |> url
        
        let (zipCode, city) = 
            match element ".item-geoloc > h2" |> read with
            | String.Regex @"([^0-9]+)\s+\(([0-9]+)" [city; zipCode] -> ZipCode zipCode, City city
            | s -> failwithf "failed to parse city %s" s
                
        { Bid = 
            { Url = uri
              ZipCode = zipCode
              City = city
              Category = Category.PropertyCategory
              Price = element ".price" |> read |> parsePrice }
          Author = "" |> Author
          Title = Title browser.Title
          Date = DateTime.UtcNow
          Datas =     
            let allData = 
                elements ".item-summary > li" 
                |> List.map read
                |> List.map (fun r -> 
                    match r.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
                    | n :: v :: _ -> n, v
                    | v -> failwith "enable to parse value %A")
                |> Map.ofList

            let l = 
                match element ".energy-box" |> elementWithin ".rank" |> htmlClass with
                | String.Regex @"rank-([A-Z]{1})" [l] -> l
                | _ -> failwith "failed to find energy class letter"
            allData |> Map.add "Classe énergie" l }

module SeLoger = 
    open SearchDomain
    open SetTheory

    let parseCity = function
        | String.Regex @"[^à].+à\s*(.*)" [city] 
        | String.Regex @"([^(]*)\s*" [city] -> city |> City
        | _ -> failwith "can't find seloger.com city from detail"
    
    let search criteria = 
//let criteria = (Price 300000m => Price 380000m := PropertyCategory, ZipCode "77000", City "Melun")
//quit ()
//start chrome

        let (category, ZipCode zipCode, City city) = criteria.Value
        let range = criteria.Interval
        
        url "http://www.seloger.com/recherche-avancee.html?idtypebien=1,2"
        element "#search_localisation" |> click
        waitFor <| fun () -> (someElement ".deploy" |> Option.isSome)
        element "#search_localisation" << zipCode
        
        waitFor <| fun () -> elements ".ui-menu-item" |> List.exists(read >> String.icontains city)
        elements ".ui-menu-item"
        |> List.filter(fun e -> e |> read |> String.icontains city)
        |> List.filter(fun e -> e |> read |> String.icontains zipCode)
        |> List.head
        |> click

        let (Price pmin) = range.Start
        let (Price pmax) = range.End
        element """input[name="prix_min"]""" << (pmin |> int |> string)
        element """input[name="prix_max"]""" << (pmax |> int |> string)
        element "#submit_ok" |> click
        element ".valid" |> click
        
        let pageCount = 
            match someElement "div.annonce__footer__pagination > p" |> Option.map read with
            | Some (String.Regex (@"[0-9]*\s*/\s*([0-9]*)") [p]) -> p |> int
            | _ -> 1

        let bids = 
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
                    if i < pageCount then
                        let next = i + 1
                        currentUrl () |> Uri |> setParameter "LISTING-LISTpg" (string next) |> string |> url
                        yield! bids next }
            bids 1
        range := { Bids = bids |> Seq.toList }

    let detail (uri:Uri) : DetailedBid = 
        uri |> string |> url
        
        let (zipCode, city) = 
            match uri.PathAndQuery.Split('/').[4] with
            | String.Regex @"([^0-9]*)-" [DataBank.CityData city] -> city
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
    open SetTheory
    open Parser

    let private location datas = 
        match datas |> Map.find "Ville" with
        | String.Regex @"([^0-9]*)\s*([0-9]*)" [city; zipCode] -> (City (city.Trim()), ZipCode zipCode)
        | l -> failwithf "failed to find city and zipcode %s" l

    let search criteria = 
        let (category, zipCode, city) = criteria.Value
        let range = criteria.Interval

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
            |> List.filter(fun e -> e.Text |> String.icontains city)
            |> List.head
            |> click

        let selectRange range = 
            let selectPriceOption v values = 
                values |> List.skipWhile(fst >> (>) v) |> List.head |> snd |> click            

            let pricesOptions selector = 
                elements selector
                |> List.collect(fun e -> 
                    match e |> read with
                    | PriceData p -> [ p, e ]
                    | _ -> [])

            pricesOptions "#ps > option" |> selectPriceOption range.Start
            pricesOptions "#pe > option" |> selectPriceOption range.End

        selectCategory cat
        selectRange range
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
            | Some pageCount -> pageCount
            | None -> 1

        let bids = 
            let rec bids i = 
                seq {
                    printfn "scanning lbc %i search page" i
                    match someElement "section.tabsContent" with
                    | Some tabs -> 
                        yield! tabs |> elementsWithin "li" |> List.map toBid
                        if i < pageCount then 
                            let next = i + 1
                            currentUrl () |> Uri |> setParameter "o" (next |> string) |> string |> url
                            yield! bids next
                    | None -> yield! Seq.empty }
            
            bids 1

        range := { Bids = bids |> Seq.toList }

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
    open SetTheory

    let cached now timeout searches = searches |> List.map(fun (x, y) -> Cache.intervalCache now timeout x, Cache.cache now timeout y)

    let crawl searches bid = 
        searches
        |> List.collect(fun (search, detail) ->
            let sr = 
                let range = 
                    let (Price p) = bid.Bid.Price
                    { Start=Price (p * 0.9m); End=Price (p * 1.1m) }
                search (range := bid.Bid.Category, bid.Bid.ZipCode, bid.Bid.City)

            sr
            |> SetTheory.toList
            |> List.collect(fun s -> s.Value.Bids)
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
                | String.Regex "([A-Z])\s" [c] -> Some c
                | _ -> None

            match k, v with
            | String.Contains "Classe énergie" _, Letter l -> Some (DataKind.EnergyClass, l)
            | _, String.Regex "DPE\s*\:\s*([A-Z])\s" [l] -> Some (DataKind.EnergyClass, l)
            | _ -> None

        let private (|GESData|_|) (k, v) = 
            let (|Letter|_|) = function
                | String.Regex "([A-Z])\s" [c] -> Some c
                | _ -> None

            match k, v with
            | _, String.Regex "GES\s*\:\s*([A-Z])\s" [l] -> Some (DataKind.GES, l)
            | String.Contains "GES" _, Letter l -> Some (DataKind.GES, l)
            | _ -> None

        let private (|NumberOfRoomsData|_|) (k,v) = 
            match k, v with
            | "Pièces", _ -> Some (DataKind.NumberOfRooms, v)
            | _, String.Regex "([0-9]+)\s*Pièces" [v] -> Some (DataKind.NumberOfRooms, v)
            | _ -> None
        
        let private (|SurfaceData|_|) (k,v) = 
            let (|Number|_|) = function
                | String.Regex "([0-9]{2,})" [surface] -> Some surface
                | _ -> None
            
            match k, v with
            | "Surface", Number n -> Some (DataKind.Surface, n)
            | _, String.Regex "Surface\s*de\s*([0-9]*)" [v] -> Some (DataKind.Surface, v)
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

let c = 
    [ //Pap.search, Pap.detail
      SeLoger.search, SeLoger.detail
      Leboncoin.search, Leboncoin.detail ]
    |> crawl


let searchResult = Pap.search(Price 300000m => Price 380000m := PropertyCategory, ZipCode "77210", City "Samoreau")
let searchResult2 = SeLoger.search (Price 300000m => Price 380000m := PropertyCategory, ZipCode "77210", City "Samoreau")
let searchResult3 = Leboncoin.search (Price 300000m => Price 380000m := PropertyCategory, ZipCode "77210", City "Samoreau")
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

//screenshot @"D:\" "log"

//let bid = "https://www.leboncoin.fr/ventes_immobilieres/950107746.htm?ca=12_s" |> Uri |> Leboncoin.detail
//let bid = "https://www.leboncoin.fr/ventes_immobilieres/985217137.htm?ca=12_s" |> Uri |> Leboncoin.detail

let bid = "https://www.leboncoin.fr/ventes_immobilieres/950107746.htm?ca=12_s" |> Uri |> Leboncoin.detail
//let bid = "https://www.leboncoin.fr/ventes_immobilieres/913646798.htm?ca=12_s" |> Uri |> Leboncoin.detail
let r = c bid
let rp = r |> Analyzer.distances Pruner.Property.prune bid
rp |> List.map fst
rp |> Analyzer.correlated |> List.map(fun (s,b) -> s, b.Bid.Url) |> List.map snd

w.Datas |> Map.toList
Pruner.Property.prune w
Pruner.Property.prune x

let one   = ("http://www.seloger.com/annonces/achat-de-prestige/maison/vulaines-sur-seine-77/110716509.htm?ci=770533&idtt=2&idtypebien=1,2&org=advanced_search&pxmin=700000" |> Uri |> SeLoger.detail)
let other = ("https://www.leboncoin.fr/ventes_immobilieres/988627556.htm?ca=12_s" |> Uri |> Leboncoin.detail) 

Analyzer.distance 
    (Pruner.Property.prune one) 
    (Pruner.Property.prune other)

//SeLoger.search (Categories.Property, ZipCode "77210", City "Samoreau", None)

quit ()

Leboncoin.search (Price.MinValue => Price.MaxValue := Category.PropertyCategory, ZipCode "77210", City "Avon")

let bagnole = 
    "https://www.leboncoin.fr/voitures/981613128.htm?ca=12_s"
    |> Uri
    |> Leboncoin.detail