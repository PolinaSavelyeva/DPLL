open System.Collections.Generic
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Literal<'Value> =
    | Pos of 'Value
    | Neg of 'Value

type Clause<'Value when 'Value: comparison> = Set<Literal<'Value>>
type Valuation<'Value when 'Value: comparison> = Set<Literal<'Value>>
type CNF<'Value when 'Value: comparison> = Set<Clause<'Value>>
    
let neg literal =
    match literal with
    | Pos l -> Neg l
    | Neg l -> Pos l

type Special =
    | PosSgn
    | NegSgn
    | NonPure
    
let specialAnd (oldValue: option<Special>) newValue =
    match oldValue, newValue with
    | None, v -> v
    | Some NonPure, _ | _, NonPure | Some PosSgn, NegSgn | Some NegSgn, PosSgn -> NonPure
    | Some PosSgn, PosSgn -> PosSgn
    | Some NegSgn, NegSgn -> NegSgn
    
let propagate cnf literals =
    let negLiterals = Set.map neg literals
    Set.fold (fun acc clause -> if (Set.intersect clause literals).IsEmpty then Set.add (clause - negLiterals) acc else acc) Set.empty cnf

let dpll cnf =
    let rec inner (cnf: CNF<int>) (valuation: Valuation<int>) =
        if cnf.IsEmpty then
            valuation
        elif cnf.Contains Set.empty then
            Set.empty
        else
            let unitClauses = Set.fold (fun acc (clause: Set<Literal<int>>) -> if clause.Count = 1 then Set.union clause acc else acc) Set.empty cnf
            
            let cnf, valuation =
                if unitClauses.IsEmpty then
                    cnf, valuation
                else
                    propagate cnf unitClauses, Set.union unitClauses valuation
            
            let folder (acc: Map<int, Special>) literal =
                match literal with
                | Pos l -> acc.Add (l, specialAnd (acc.TryFind l) PosSgn)
                | Neg l ->  acc.Add (l, specialAnd (acc.TryFind l) NegSgn) 

            let literalsInUse = Set.fold (Set.fold folder) Map.empty cnf
            let pureLiterals = Map.fold (fun acc key special -> match special with
                                                                | PosSgn -> Set.add (Pos key) acc 
                                                                | NegSgn -> Set.add (Neg key) acc
                                                                | _ -> acc) Set.empty literalsInUse
            let cnf, valuation =
                if pureLiterals.IsEmpty then
                    cnf, valuation
                else
                    Set.filter (fun clause -> (Set.intersect clause pureLiterals).IsEmpty) cnf, Set.union pureLiterals valuation
                    
            if cnf.IsEmpty then
                valuation
            elif cnf.Contains Set.empty then
                Set.empty
            else
                let fstLiteral = Seq.head cnf |> Seq.head
                let res = inner (propagate cnf (Set.singleton fstLiteral)) (Set.add fstLiteral valuation)

                if res.IsEmpty then
                    inner (propagate cnf (Set.singleton (neg fstLiteral))) (Set.add (neg fstLiteral) valuation)
                else
                    res

    inner cnf Set.empty

type DIMACSFile(pathToFile: string) =
    let rawLines = System.IO.File.ReadLines pathToFile
    let noCommentsLines = Seq.skipWhile (fun (n: string) -> n[0] = 'c') rawLines
    let header = (Seq.head noCommentsLines).Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
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
            |> Set.ofArray

        Seq.map lineMapping data |> Set

let toDIMACSOutput (valuation: Valuation<int>) varsNum =
    if valuation.IsEmpty then
        "s UNSATISFIABLE"
    else
        let ansArray = [|1 .. varsNum|]
        Set.iter (fun literal -> match literal with
                                 | Neg l -> ansArray[l - 1] <- -ansArray[l - 1]
                                 | _ -> () )  valuation
        Array.fold
            (fun acc value ->
                let sep = if abs value % 40 = 0 then "\nv " else " "
                acc + string value + sep)
            "s SATISFIABLE\nv "
            ansArray
        + "0"

let solve pathToFile =
    let file = DIMACSFile(pathToFile)
    let model = dpll file.ToCNF

    printfn $"%s{toDIMACSOutput model file.VarsNum}"

[<EntryPoint>]
let main args =
    // if Array.length args = 1 then
    //     solve args[0]
    //     0
    // else
    //     printfn "Pass the one path to DIMACS file as an input argument"
    //     -1
   //0
    solve "/Users/svmena/Documents/MySat/examples/aim-100-1_6-no-1.cnf"
    0