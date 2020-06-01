module ICPC
open System

type 'a option = 
|None
|Some



let _isLower (str: string) = 
    let rec strIter isLower arr = 
        match arr with 
        | [] -> isLower
        |_ -> 
            match Char.IsUpper(arr.Head) with
            | true -> strIter false []
            | false -> strIter true arr.Tail
    strIter true (Array.toList <| str.ToCharArray())

let matchChar (str: string) =
    let rec puncMatch count (strIn: string) =
       // let useStr = (str.Chars(count))
        match (strIn.Chars(count)) with
        |','|'.'|' '|'a' -> puncMatch (count+1) strIn = true
        |_ -> false
    puncMatch 0 str

let convertStringComma sentence = sentence.ToString() + ", "

//let convertStringNormal normSentence = 

let rec CheckMethod input n a =

    let splitLineComma = (fun (line:string) -> Seq.toList (line.Split ','))
    let commaString = splitLineComma input 

    let Cnew = commaString.ToString()

    let splitLine = (fun (line:string) -> Seq.toList (line.Split ' '))
    let initialString = splitLine Cnew

    match initialString.[a] = commaString.[n] && a <> initialString.Length + 1 && n <> initialString.Length + 1 with  
    |true -> convertStringComma initialString.[a], a + 1
    |false -> n+1
    
    //CheckMethod stringInput 0 0 

let commaSprinkler input =
    let strInput = input.ToString()
    let length = strInput.Length
    match length>= 2 && matchChar strInput && _isLower strInput with
    |true -> CheckMethod strInput 0 0 
    |false -> None


  
    //failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =

    printfn "Hello World from F#!"
    0 // return an integer exit code
