#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""

open canopy
open runner
open System

start chrome
url "http://www.geny.com/"

let href (a:OpenQA.Selenium.IWebElement) = a.GetAttribute("href")

let contains (search:string) (source:String) = source.IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1

let links = elements "a" |> List.filter(fun a -> a |> href |> contains "#reunion") |> List.map href

let link = links |> List.head

links |> List.head |> url

elements "cotes" |> List.head |> click

elements "#arrivees tr" |> List.map(elementsWithin "td" >> List.map read)

quit ()
