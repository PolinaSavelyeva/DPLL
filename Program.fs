open System.Collections.Generic
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Literal<'Value> =
    | Pos of 'Value
    | Neg of 'Value

type Clause<'Value> = list<Literal<'Value>>
type Valuation<'Value> = list<Literal<'Value>>
type CNF<'Value> = list<Clause<'Value>>
    
let neg literal =
    match literal with
    | Pos l -> Neg l
    | Neg l -> Pos l

type Special =
    | None
    | PosSign
    | NegSign
    | NonPure
    
let specialAnd oldValue newValue =
    match oldValue, newValue with
    | None, v -> v
    | NonPure, _ | _, NonPure | PosSign, NegSign | NegSign, PosSign -> NonPure
    | PosSign, PosSign -> PosSign
    | NegSign, NegSign -> NegSign
    | _ -> failwith "Unexpected None as a second argument"
    
let propagate cnf unitClauses negUnitClauses =
    List.choose (fun clause -> let clauseSet = Set.ofList clause
                               if (clauseSet - unitClauses).IsEmpty then
                                  Some (clauseSet - negUnitClauses |> Set.toList)
                               else
                                  Option.None) cnf

let dpll cnf =
    let rec inner cnf valuation =
        if List.isEmpty cnf then
            valuation
        elif List.contains [] cnf then
            []
        else
            let unitClauses = List.choose (fun clause -> if List.length clause = 1 then Some (List.head clause) else Option.None) cnf |> Set.ofList
            let negUnitClauses = Set.map neg unitClauses
            let cnf = propagate cnf unitClauses negUnitClauses
            
            let folder (acc: Map<int, Special>) literal =
                match literal with
                | Pos l -> acc.Change (l, (fun _ -> Some (specialAnd PosSign acc[l])))
                | Neg l -> acc.Change (l, (fun _ -> Some (specialAnd NegSign acc[l])))

            let literalsInUse = List.fold (List.fold folder) Map.empty cnf
            let pureLiteralsSet = Map.fold (fun acc key special -> match special with
                                                              | PosSign -> acc @ [Pos key]
                                                              | NegSign -> acc @ [Neg key]) [] literalsInUse |> Set.ofList
            let cnf = List.filter (fun clause -> (Set.ofList clause - pureLiteralsSet).IsEmpty) cnf         

            let fstLiteral = List.head cnf |> List.head
            let res = inner (propagate cnf (Set [fstLiteral]) (Set [neg fstLiteral]) ) (valuation @ [ fstLiteral ])

            if List.isEmpty res then
                inner (propagate cnf (Set [neg fstLiteral]) (Set [fstLiteral])) (valuation @ [ neg fstLiteral ])
            else
                res

    inner cnf []

type DIMACSFile(pathToFile: string) =
    let rawLines = System.IO.File.ReadLines pathToFile
    let noCommentsLines = Seq.skipWhile (fun (n: string) -> n[0] = 'c') rawLines
    let header = (Seq.head noCommentsLines).Split()
    let varsNum = int header[2]
    let clausesNum = int header[3]
    let data = Seq.tail noCommentsLines

    member this.Data = data
    member this.VarsNum = varsNum
    member this.ClausesNum = clausesNum

    member this.ToCNF =

        let lineMapping (line: string) =
            Array.takeWhile ((<>) "0") (line.Split())
            |> Array.map (fun n ->
                let n = n |> int
                if n < 0 then Neg -n else Pos n)
            |> Array.toList

        Seq.map lineMapping data |> Seq.toList

let toDIMACSOutput valuation varsNum =
    match valuation with
    | [] -> "s UNSATISFIABLE"
    | _ ->

        let rec normalizeValuation curNum ans =
            if curNum > varsNum then
                ans
            elif
                (List.tryFind
                    (fun literal ->
                        match literal with
                        | Pos l
                        | Neg l -> l = curNum)
                    valuation)
                    .IsNone
            then
                normalizeValuation (curNum + 1) (ans @ [ Pos curNum ])
            else
                normalizeValuation (curNum + 1) ans

        let valuation =
            List.sortBy
                (fun literal ->
                    match literal with
                    | Pos l
                    | Neg l -> l)
                (normalizeValuation varsNum valuation)

        List.fold
            (fun acc literal ->
                let literalVal =
                    match literal with
                    | Neg l -> -l
                    | Pos l -> l

                let sep = if String.length acc % 77 = 0 then "\nv " else " "
                acc + string literalVal + sep)
            "s SATISFIABLE\nv "
            valuation
        + "0"

let solve pathToFile =
    let file = DIMACSFile(pathToFile)
    let model = dpll file.ToCNF

    printfn $"%s{toDIMACSOutput model file.VarsNum}"

[<EntryPoint>]
let main args =
    if Array.length args = 1 then
        solve args[0]
        0
    else
        printfn "Pass the one path to DIMACS file as an input argument"
        -1
