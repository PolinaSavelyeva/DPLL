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
    List.filter (fun clause -> clause <> [ literal ] && (List.contains literal clause |> not)) cnf
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

[<EntryPoint>]
let main args = 0
