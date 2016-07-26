#r """..\packages\FSharp.Data\lib\net40\FSharp.Data.dll"""

open FSharp.Data

let [<Literal>] json = """{"value":"hello"}"""

type JsonApi = JsonProvider< json > 
