open Microsoft.FSharp.Collections

type Literal =
    | Pos of int
    | Neg of int

type Clause = list<Literal>
type Valuation = list<Literal>
type CNF = list<Clause>

let neg literal =
    match literal with
    | Pos l -> Neg l
    | Neg l -> Pos l

type Special =
    | PosSgn
    | NegSgn
    | NonPure

let specialAnd oldValue newValue =
    match oldValue, newValue with
    | None, v -> v
    | Some NonPure, _
    | Some PosSgn, NegSgn
    | Some NegSgn, PosSgn -> NonPure
    | Some PosSgn, PosSgn -> PosSgn
    | Some NegSgn, NegSgn -> NegSgn
    | _, NonPure -> failwith "Unexpected NonPure as a second argument"

let propagate cnf literals =
    let literalsSet = Set.ofList literals
    let negLiteralsSet = Set.map neg literalsSet

    List.choose
        (fun clause ->
            let clauseSet = Set.ofList clause

            if (Set.intersect clauseSet literalsSet).IsEmpty then
                Some(clauseSet - negLiteralsSet |> Set.toList)
            else
                Option.None)
        cnf

let dpll cnf =
    let rec inner (cnf: CNF) (valuation: Valuation) =
        if cnf.IsEmpty then
            valuation
        elif List.contains [] cnf then
            []
        else
            let unitClauses =
                List.fold
                    (fun acc clause ->
                        if List.length clause = 1 then
                            List.head clause :: acc
                        else
                            acc)
                    []
                    cnf

            if List.exists (fun unitClause -> List.contains (neg unitClause) unitClauses) unitClauses then // forall
                []
            else
                let cnf, valuation =
                    if unitClauses.IsEmpty then
                        cnf, valuation
                    else
                        propagate cnf unitClauses, unitClauses @ valuation

                let folder (acc: Map<int, Special>) literal =
                    match literal with
                    | Pos l -> acc.Add(l, specialAnd (acc.TryFind l) PosSgn)
                    | Neg l -> acc.Add(l, specialAnd (acc.TryFind l) NegSgn)

                let literalsInUse = List.fold (List.fold folder) Map.empty cnf // List.collect

                let pureLiterals =
                    Map.fold
                        (fun acc key special ->
                            match special with
                            | PosSgn -> Pos key :: acc
                            | NegSgn -> Neg key :: acc
                            | _ -> acc)
                        []
                        literalsInUse

                let pureLiteralsSet = Set.ofList pureLiterals

                let cnf, valuation =
                    if pureLiterals.IsEmpty then
                        cnf, valuation
                    else
                        List.filter
                            (fun clause ->
                                let clauseSet = clause |> Set.ofList
                                (Set.intersect clauseSet pureLiteralsSet).IsEmpty)
                            cnf,
                        pureLiterals @ valuation

                if cnf.IsEmpty then
                    valuation
                elif List.contains [] cnf then
                    []
                else
                    let fstLiteral = List.head cnf |> List.head

                    let res = inner (propagate cnf [ fstLiteral ]) (fstLiteral :: valuation)

                    if res.IsEmpty then
                        let negFstLiteral = neg fstLiteral
                        inner (propagate cnf [ negFstLiteral ]) (negFstLiteral :: valuation)
                    else
                        res

    inner cnf []

type DIMACSFile(pathToFile: string) =
    let rawLines = System.IO.File.ReadLines pathToFile
    let noCommentsLines = Seq.skipWhile (fun (n: string) -> n[0] = 'c') rawLines

    let header =
        (Seq.head noCommentsLines)
            .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)

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
            |> List.ofArray

        Seq.map lineMapping data |> Seq.toList

let toDIMACSOutput (valuation: Valuation) varsNum =
    if valuation.IsEmpty then
        "s UNSATISFIABLE"
    else
        let ansArray = [| 1..varsNum |]

        List.iter
            (fun literal ->
                match literal with
                | Neg l -> ansArray[l - 1] <- -ansArray[l - 1]
                | _ -> ())
            valuation

        Array.fold
            (fun (acc: string) value ->
                let sep = if (acc.Length - 15) % 40 = 0 then "\nv " else " "
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
    if Array.length args = 1 then
        solve args[0]
        0
    else
        printfn "Pass the one path to DIMACS file as an input argument"
        -1
