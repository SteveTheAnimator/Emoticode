open System.Collections.Generic
open System.Text.RegularExpressions
open System

type Value =
    | Num of int
    | Text of string

let variables = Dictionary<string, Value>()
let functions = Dictionary<string, string list>()

let trimBookEmoji (s: string) =
    let book = "-"
    let trimmed = 
        if s.StartsWith(book) && s.EndsWith(book) && s.Length >= 2 then
            s.Substring(1, s.Length - 2)
        else s
    trimmed

let substituteVariables (text: string) =
    let regex = Regex(@"\$(\w+)")
    let result = regex.Replace(text, fun m ->
        let varName = m.Groups.[1].Value
        match variables.TryGetValue(varName) with
        | true, Num n -> n.ToString()
        | true, Text t -> t
        | _ -> m.Value)
    result

let rec interpret (lines: string list) =
    let mutable i = 0
    while i < lines.Length do
        let line = lines.[i].Trim()
        if String.IsNullOrWhiteSpace(line) then
            i <- i + 1
        else
            match line.Split([|' '|], 2) |> Array.toList with
            | ["🖨️"; rest] when rest.StartsWith("-") && rest.EndsWith("-") ->
                let content = trimBookEmoji rest |> substituteVariables
                printfn "%s" content
                i <- i + 1
            | _ ->
                match line.Split(' ') |> Array.toList with
                | ["🧾"; var; value] ->
                    match Int32.TryParse(value) with
                    | true, n -> variables.[var] <- Num n
                    | _ -> 
                        if value.StartsWith("$") then
                            let varName = value.Substring(1)
                            match variables.TryGetValue(varName) with
                            | true, v -> variables.[var] <- v
                            | _ -> variables.[var] <- Text value
                        else
                            variables.[var] <- Text value
                    i <- i + 1
                | ["➕"; var; value] ->
                    match variables.TryGetValue(var) with
                    | true, Num n ->
                        match Int32.TryParse(value) with
                        | true, v -> variables.[var] <- Num (n + v)
                        | _ -> 
                            if value.StartsWith("$") then
                                let varName = value.Substring(1)
                                match variables.TryGetValue(varName) with
                                | true, Num v -> variables.[var] <- Num (n + v)
                                | _ -> ()
                            else ()
                    | _ -> ()
                    i <- i + 1
                | ["➖"; var; value] ->
                    match variables.TryGetValue(var) with
                    | true, Num n ->
                        match Int32.TryParse(value) with
                        | true, v -> variables.[var] <- Num (n - v)
                        | _ -> 
                            if value.StartsWith("$") then
                                let varName = value.Substring(1)
                                match variables.TryGetValue(varName) with
                                | true, Num v -> variables.[var] <- Num (n - v)
                                | _ -> ()
                            else ()
                    | _ -> ()
                    i <- i + 1
                | ["🔁"; countStr] ->
                    match Int32.TryParse(countStr) with
                    | true, count ->
                        let block = ResizeArray<string>()
                        i <- i + 1
                        while i < lines.Length && lines.[i].Trim() <> "🔚" do
                            block.Add(lines.[i])
                            i <- i + 1
                        for _ in 1 .. count do
                            interpret (block |> Seq.toList)
                        i <- i + 1
                    | _ -> 
                        if countStr.StartsWith("$") then
                            let varName = countStr.Substring(1)
                            match variables.TryGetValue(varName) with
                            | true, Num count ->
                                let block = ResizeArray<string>()
                                i <- i + 1
                                while i < lines.Length && lines.[i].Trim() <> "🔚" do
                                    block.Add(lines.[i])
                                    i <- i + 1
                                for _ in 1 .. count do
                                    interpret (block |> Seq.toList)
                                i <- i + 1
                            | _ -> failwith "Invalid loop count variable"
                        else 
                            failwith "Invalid loop count"
                | ["🤔"; var; op; value] ->
                    let condition =
                        match variables.TryGetValue(var) with
                        | true, Num n ->
                            match Int32.TryParse(value) with
                            | true, v ->
                                match op with
                                | "==" -> n = v
                                | "!=" -> n <> v
                                | ">" -> n > v
                                | "<" -> n < v
                                | ">=" -> n >= v
                                | "<=" -> n <= v
                                | _ -> false
                            | _ -> 
                                if value.StartsWith("$") then
                                    let varName = value.Substring(1)
                                    match variables.TryGetValue(varName) with
                                    | true, Num v ->
                                        match op with
                                        | "==" -> n = v
                                        | "!=" -> n <> v
                                        | ">" -> n > v
                                        | "<" -> n < v
                                        | ">=" -> n >= v
                                        | "<=" -> n <= v
                                        | _ -> false
                                    | _ -> false
                                else
                                    false
                        | _ -> false
                    i <- i + 1
                    if condition then
                        let block = ResizeArray<string>()
                        while i < lines.Length && lines.[i].Trim() <> "🔚" do
                            block.Add(lines.[i])
                            i <- i + 1
                        interpret (block |> Seq.toList)
                    else
                        while i < lines.Length && lines.[i].Trim() <> "🔚" do
                            i <- i + 1
                    i <- i + 1 
                | ["🧠"; funcName] ->
                    let block = ResizeArray<string>()
                    i <- i + 1
                    while i < lines.Length && lines.[i].Trim() <> "🔚" do
                        block.Add(lines.[i])
                        i <- i + 1
                    functions.[funcName] <- block |> Seq.toList
                    i <- i + 1
                | ["📞"; funcName] ->
                    match functions.TryGetValue(funcName) with
                    | true, funcLines -> interpret funcLines
                    | _ -> failwithf "Function %s not defined" funcName
                    i <- i + 1
                | _ -> i <- i + 1

let script = [
    "🧾 x 5"
    "🧾 y 10"
    "🤔 x < y"
    "🖨️ -x is less than y-"
    "🔚"
    "🔁 3"
    "🖨️ -Looping...-"
    "🔚"
    "🧠 greet"
    "🖨️ -Hello from function!-"
    "🔚"
    "➕ x 3"
    "🖨️ -x is now $x-"
    "🧾 counter $x"
    "🔁 $counter"
    "🖨️ -Counter loop iteration-"
    "🔚"
    "📞 greet"
]

interpret script
