module String
open System
open System.Text.RegularExpressions

let icontains (search:string) (source:string) = 
    match search, source with
    | null, null -> true
    | _, null | null, _ -> false
    | _ -> source.IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1

let (|Contains|_|) search source = 
    if icontains search source then Some source
    else None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None