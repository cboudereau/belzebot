#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""
#r """..\packages\FSharp.Data\lib\net40\FSharp.Data.dll"""
#r """..\packages\Outatime\lib\net452\Outatime.dll"""

#load "String.fs"
#load "canopy.fs"

open System

open canopy
open runner

start chrome

type Make = Make of string
type Model = Model of string
type MaxKm = MaxKm of int
type Power = Power of int

type Car = 
    { Make:Make
      Model:Model
      MaxKm:MaxKm
      Power:Power }

type Price = Price of int
type Km = Km of int
type Year = Year of int

type FoundCar = 
    { Url:Uri
      Model:Model
      Make:Make
      Price: Price
      Km: Km
      Year:Year }

let parseNumber source = 
    match if String.IsNullOrEmpty(source) then String.Empty else source.Replace(" ", "") with
    | String.Regex "([0-9]*)" [n] -> n |> int
    | other -> failwithf "enable to parse %s to int" other 
    

fsi.AddPrinter<Uri>(fun uri -> uri.ToString())

module LaCentrale = 
    let search car = 
        let (Power power) = car.Power
        let (Make make) = car.Make
        let (Model model) = car.Model
        let (MaxKm maxKm) = car.MaxKm
        
        sprintf "http://www.lacentrale.fr/listing_auto.php?tri=&sens=&multicriteres=%s%%7C%s&SS_CATEGORIE=40%%2C41%%2C42%%2C43%%2C44%%2C45%%2C46%%2C47%%2C48%%2C49&marque=CITROEN&modele=PICASSO&energie=2&prix_mini=&prix_maxi=&region=&cp=&dptCp=&annee=&annee2=&km_mini=&km_maxi=%i&conso=&co2=&opt=&version=%i&transmission=&couleur=&nbportes=&length=&photo=&new_annonce=&origine=&reference=" make model maxKm power
        |> url

        elements ".resultList > .adLineContainer"
        |> List.map (fun e -> 
            { Url = e |> elementWithin ".linkAd" |> href |> Uri
              Model= Model model
              Make = Make make
              Km = e |> elementWithin ".fieldMileage" |> read |> parseNumber |> Km
              Year = e |> elementWithin ".fieldYear" |> read |> int |> Year
              Price = e |> elementWithin ".fieldPrice" |> read |> parseNumber |> Price })

let picasso = { Make=Make "Citroen"; Model=Model "Picasso"; MaxKm=MaxKm 100000; Power=Power 110; }

LaCentrale.search picasso

