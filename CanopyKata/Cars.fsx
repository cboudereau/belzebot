#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""
#r """..\packages\FSharp.Data\lib\net40\FSharp.Data.dll"""
#r """..\packages\Outatime\lib\net452\Outatime.dll"""
#r """..\packages\Topshelf.FSharp\lib\net40\Topshelf.FSharp.dll"""
#r """..\packages\Topshelf\lib\net452\Topshelf.dll"""

#load "String.fs"
#load "canopy.fs"
#load "Slack.fs"

open System

open canopy
open runner

type Make = Make of string
type Model = Model of string
type MaxKm = MaxKm of int
type Power = Power of int
type Price = Price of int

type Car = 
    { Make:Make
      Model:Model
      MaxKm:MaxKm
      Power:Power
      Price:Price }

type Km = Km of int
type Year = Year of int

type FoundCar = 
    { Url:string
      Model:Model
      Make:Make
      Price: Price
      Km: Km
      Year:Year }

let parseNumber source = 
    try
        System.String((source:string) |> Seq.filter(Char.IsNumber) |> Seq.toArray) |> int
    with ex -> 
        printfn "failed to parse %s" source
        reraise ()

//fsi.AddPrinter<Uri>(fun uri -> uri.ToString())

module LaCentrale = 
    let search car = 
        let (Power power) = car.Power
        let (Make make) = car.Make
        let (Model model) = car.Model
        let (MaxKm maxKm) = car.MaxKm
        
        sprintf "http://www.lacentrale.fr/listing_auto.php?tri=&sens=&multicriteres=%s%%7C%s&SS_CATEGORIE=40%%2C41%%2C42%%2C43%%2C44%%2C45%%2C46%%2C47%%2C48%%2C49&marque=%s&modele=%s&energie=2&prix_mini=&prix_maxi=&region=&cp=&dptCp=&annee=&annee2=&km_mini=&km_maxi=%i&conso=&co2=&opt=&version=%i&transmission=&couleur=&nbportes=&length=&photo=&new_annonce=&origine=&reference=" make model make model maxKm power 
        |> url

        if elements ".clear" |> List.exists(read >> String.icontains "aucun") then []
        else
            let rec bids () = 
                seq {
                    yield!
                        elements ".adContainer"
                        |> Seq.map (fun e -> 
                            { Url = e |> elementWithin ".linkAd" |> href
                              Model= Model model
                              Make = Make make
                              Km = e |> elementWithin ".fieldMileage" |> read |> parseNumber |> Km
                              Year = e |> elementWithin ".fieldYear" |> read |> int |> Year
                              Price = e |> elementWithin ".fieldPrice" |> read |> parseNumber |> Price })
                    
                    let last = element "li.last > a"
                    
                    if last |> attribute "title" |> String.icontains "Page suivante" then 
                        last |> href |> url 
                        yield! bids () }
            bids () |> Seq.toList

module AutoScoot24 = 
    let private pruneModel = function
        | String.Contains "picasso" _ -> "Xsara picasso"
        | other -> other
    
    let search car = 
        let (Power power) = car.Power
        let (Make make) = car.Make
        let (Model model) = car.Model
        let (MaxKm maxKm) = car.MaxKm
        
        url "http://ww3.autoscout24.fr/avancee?cy=F&ustate=N%2CU&intcidm=HP-Searchmask-Extendedsearch#atype=C&cy=F&ustate=N%2CU"

        waitFor <| fun () -> "#make0 > option" |> elements |> List.length > 2

        "#make0 > option"
        |> elements
        |> List.filter(fun e -> e |> read |> String.icontains make)
        |> List.head
        |> click

        waitFor <| fun () -> elements ".inputFullWidth > option" |> List.length > 1

        "#model0 > option"
        |> elements
        |> List.filter(fun e -> e |> read |> String.icontains (model |> pruneModel))
        |> List.head
        |> click

        element "#openFueltypes" |> click
        waitFor <| fun () -> (element "#dieselText").Displayed
        element "#dieselText" |> click

        element "#powerFrom" << string (power - 5)
        
        elements "#powerUnit > option"
        |> List.filter(fun e-> e |> read |> String.icontains "ch")
        |> List.head
        |> click

        elements "#mileageTo > option"
        |> List.tail
        |> List.skipWhile(fun e -> e |> read |> parseNumber < maxKm)
        |> List.head
        |> click

        element "#searchLinkTop" |> click
        
        match someElement "#searchLinkTop" with
        | Some e when e |> read |> String.icontains "0" -> []
        | _ ->
            let rec bids () =
                seq {
                    yield!
                        elements "div > .articleListItem"
                        |> List.map(fun e ->
                            let raw = (e |> read).Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                            { Url = e |> elementWithin "a.adInLink" |> href
                              Model= Model model
                              Make = Make make
                              Km = raw |> List.map(function String.Regex "([0-9\s]*)\s*km" [km] -> Some km | _ -> None) |> List.map Option.toList |> List.concat |> List.head |> parseNumber |> Km
                              Year = 
                                raw 
                                |> List.map(function 
                                    | String.Regex "[0-9]{2}/([0-9]{4})" [year] -> Some year 
                                    | String.Contains "-/-" _ -> System.DateTime.UtcNow.Year |> string |> Some
                                    | _ -> None) |> List.map Option.toList |> List.concat |> List.head |> parseNumber |> Year
                              Price = raw |> List.filter(fun l -> l |> String.icontains "€") |> List.head |> parseNumber |> Price } ) 
                    
                    let next = element "a.rightArrowClick"
                    if next |> attribute "class" |> String.icontains "disabled" then
                        next |> click
                        yield! bids () }

            bids () |> Seq.toList

module OccasionDuLion =
    let private pruneMake = function
        | String.Contains "citroen" _ -> "Citroën"
        | other -> other
    
    let private pruneModel = function
        | String.Contains "picasso" _ -> "Xsara picasso"
        | other -> other

    let search car = 
        let (Power power) = car.Power
        let (Make make) = car.Make
        let (Model model) = car.Model
        let (MaxKm maxKm) = car.MaxKm

        url "http://www.occasionsdulion.com/recherche-detaillee"
        
        element "#searchEnginProductListBrand" |> click
        
        elements "#searchEnginProductListBrand > option"
        |> List.filter(fun e -> e |> read |> String.icontains (pruneMake make))
        |> List.head
        |> click

        element "#searchEnginProductListModel" |> click
        elements "#searchEnginProductListModel > option"
        |> List.filter(fun e -> e |> read |> String.icontains (pruneModel model))
        |> List.head
        |> click

        element "#searchEnginProductListEnergy" |> click
        elements "#searchEnginProductListEnergy > option"
        |> List.filter(fun e -> e |> read |> String.icontains "Diesel")
        |> List.head
        |> click
        
        element "#searchEnginProductListPower" |> click
        elements "#searchEnginProductListPower > option"
        |> List.tail
        |> List.filter(fun e -> 
            match e |> attribute "value" with
            | String.Regex "\[([0-9]*)\s*TO\s*([0-9]*)\]" [f;t] -> 
                (f |> int) <= power && power <= (t |> int) 
            | other -> failwithf "enable to select power : %s" other)
        |> List.head
        |> click

        element "#MileageMaxVal" << string maxKm

        press enter

        if elements "u" |> List.exists (fun e -> e |> read |> String.icontains "aucun") then []
        else
            elements ".moduleResultat > ul > li"
            |> List.map(fun e -> 
                let raw = (e |> read).Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                { Url = 
                    let rel = (e |> link).Replace("javascript:setPostUrl('", "").Replace("')", "")
                    sprintf "http://www.occasionsdulion.com%s" rel
                  Model= Model model
                  Make = Make make
                  Km = raw |> List.map(function String.Regex "([0-9\s]*)\s*kms" [km] -> Some km | _ -> None) |> List.map Option.toList |> List.concat |> List.head |> parseNumber |> Km
                  Year = raw |> List.map(function String.Regex "([0-9]{4})" [year] -> Some year | _ -> None) |> List.map Option.toList |> List.concat |> List.head |> parseNumber |> Year
                  Price = raw |> List.filter(fun l -> l |> String.icontains "€") |> List.head |> parseNumber |> Price })

module CitroenSelect = 
    let private pruneMake = function
        | String.Contains "citroen" _ -> "Citroën"
        | other -> other
    
    let pruneModel = function
        | String.Contains "picasso" _ -> "Xsara picasso"
        | other -> other

    let search car = 
        let (Power power) = car.Power
        let (Make make) = car.Make
        let (Model model) = car.Model
        let (MaxKm maxKm) = car.MaxKm
        let (Price price) = car.Price

        url "http://www.citroenselect.fr/"

        elements "#ddlModels > option"
        |> List.filter(fun e -> e |> read |> String.icontains (model |> pruneModel))
        |> List.head
        |> click

        elements "#ddlEnergies > option"
        |> List.filter(read >> String.icontains "Diesel")
        |> List.head
        |> click

        element "#txtMileageMax" << string maxKm
        
        element "#txtPriceMax" << string price

        element "#search" |> click

        if elements "h2" |> List.exists(fun e -> e |> read |> String.icontains "0") then []
        else
            let rec bids () = 
                seq {
                    yield! 
                        elements "#results-list > a"
                        |> List.map(fun e ->
                            let raw = (e |> read).Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                            let p = raw |> List.filter(String.icontains "ch") |> List.head |> parseNumber
                            p,
                            { Url = e |> href
                              Model= Model model
                              Make = Make make
                              Km = raw |> List.map(function String.Regex "([0-9\s]+)[^a-z]\s*km" [km] -> Some km | _ -> None) |> List.map Option.toList |> List.concat |> List.head |> parseNumber |> Km
                              Year = raw |> List.map(function String.Regex "([0-9]{4})" [year] -> Some year | _ -> None) |> List.map Option.toList |> List.concat |> List.head |> parseNumber |> Year
                              Price = raw |> List.filter(fun l -> l |> String.icontains "€") |> List.head |> parseNumber |> Price })
                        |> List.filter(fun (x,_) -> x > (power - 5))
                        |> List.map snd
                    
                    let next = 
                        elements ".pagination > li"
                        |> List.filter(fun e -> e |> read |> String.icontains "»")
                        |> List.head
                        
                    let hasNext = next |> attribute "class" |> String.icontains "disabled" |> not
                    if hasNext then 
                        next |> link |> url
                        yield! bids () }
            bids () |> Seq.toList

let c3 = { Make=Make "Citroen"; Model=Model "C3"; MaxKm=MaxKm 40000; Power=Power 60; Price= Price 1000 }

start chrome
AutoScoot24.search c3
CitroenSelect.search c3
OccasionDuLion.search c3
LaCentrale.search c3
quit ()

open Topshelf
open Topshelf.FSharpApi
open System.Threading

let cts = new CancellationTokenSource ()

let tryF f =
    async {
        try
            do! f()
        with ex -> 
            ex
            |> sprintf "@clem : belzebot is dead with error %O"
            |> Slack.post (Slack.Channel "#c3") 
            |> Async.RunSynchronously }

let startService _ = 
    let rec scan previous = 
        async {
            start chrome
            let r = CitroenSelect.search c3 
            let bids = r |> Set.ofList
            let news = Set.difference bids previous
            
            printfn "%i found" (news |> Seq.length)

            news
            |> Set.map(fun b -> 
                let (Model model) = b.Model
                let (Make make) = b.Make
                let (Km km) = b.Km
                let (Price price) = b.Price 
                let (Year year) = b.Year
                sprintf "@clem found : %s %s (%i) with %iKm, %i€ @ %O" make model year km price b.Url)
            |> Set.iter (Slack.post (Slack.Channel "#c3") >> Async.RunSynchronously)
                
            quit ()
            do! Async.Sleep(10*1000)
            do! scan (bids |> Set.union previous) }
    Async.Start(tryF (fun () -> scan Set.empty), cts.Token)
    true

let stopService _ = 
    cts.Cancel()
    true

Service.Default
|> with_start startService
|> with_stop stopService
|> with_recovery (ServiceRecovery.Default |> restart (TimeSpan.FromSeconds(10.)))
|> run

start chrome

CitroenSelect.search c3

LaCentrale.search c3

AutoScoot24.search c3 |> List.length

CitroenSelect.search c3
OccasionDuLion.search c3


start chrome

url "http://www.google.fr"

describe "Searching from google"

puts "hello"

highlight ".gsfi"

element ".gsfi" << "hello world"

press enter

elements "h3" |> List.map read

start chrome
let browser1 = browser
start chrome
let browser2 = browser
start chrome
let browser3 = browser

tile [browser1; browser2; browser3]


quit ()