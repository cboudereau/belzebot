#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""

open canopy
open runner
open System
open OpenQA.Selenium
open OpenQA.Selenium.Remote

start chrome
pin direction.Right
url """https://www.zone-telechargement.com/jeux/ps3/48316-metal-gear-solid-v-ground-zeroes-ps3.html"""

let elts = elements "#news-id-48316 span" 

let position = 
    elts
    |> Seq.indexed
    |> Seq.filter(fun (_,e) -> e.Text = "Uplea") 
    |> Seq.head

let links = 
    elts 
    |> Seq.filter(fun e -> e.Text = "Uplea") 
    |> Seq.head
    |> parent
    |> elementsWithin "*"
    |> Seq.skipWhile(fun e -> e.Text <> "Uplea")
    |> Seq.filter(fun e -> e.TagName = "a")
    |> Seq.head
    |> click


quit ()
