#r """..\packages\Selenium.WebDriver\lib\net40\WebDriver.dll"""
#r """..\packages\canopy\lib\canopy.dll"""

open canopy
open runner
open System

start chrome
pin Right
url "http://www.google.fr"

".gsfi" << "hello"
press enter

elements ".r a" |> Seq.head |> click

quit ()

