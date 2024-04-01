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

let propagate literal cnf =
    List.filter (fun clause -> List.contains literal clause |> not) cnf
    |> List.map (List.filter ((<>) (neg literal)))

let dpll cnf =
    let rec inner cnf valuation =
        if List.isEmpty cnf then
            valuation
        elif List.contains [] cnf then
            []
        else
            let unitClause = List.tryFind (fun clause -> List.length clause = 1) cnf

            if unitClause.IsSome then
                let unitLiteral = unitClause.Value |> List.head
                inner (propagate unitLiteral cnf) (valuation @ [ unitLiteral ])
            else
                let fstLiteral = List.head cnf |> List.head
                let res = inner (propagate fstLiteral cnf) (valuation @ [ fstLiteral ])

                if List.isEmpty res then
                    inner (propagate (neg fstLiteral) cnf) (valuation @ [ neg fstLiteral ])
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
