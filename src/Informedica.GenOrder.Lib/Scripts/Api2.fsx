
// load demo or product cache
System.Environment.SetEnvironmentVariable("GENPRES_PROD", "1")
System.Environment.SetEnvironmentVariable("GENPRES_URL_ID", "1IZ3sbmrM4W4OuSYELRmCkdxpN9SlBI-5TLSvXWhHVmA")


#load "load.fsx"


#time


open MathNet.Numerics
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib


module DoseLimit = DoseRule.DoseLimit


let calcTotal () =
    Patient.patient
    |> PrescriptionRule.get
    |> Array.collect (fun pr ->
        pr.DoseRule.DoseLimits
        |> Array.collect _.Products
        |> Array.filter (fun p -> p.Generic |> String.contains "/")
        |> Array.collect (fun p ->
            pr.DoseRule.Frequencies
            |> Option.map (ValueUnit.get >> fst)
            |> Option.defaultValue [| 1N |]
            |> Array.collect (fun _ ->
                if pr.SolutionRules |> Array.isEmpty then [| 1N |]
                else
                    pr.SolutionRules
                    |> Array.map (fun _ -> 1N)

            )
        )
    )
    |> Array.length


let calcTotal2 () =
    let dsrs =
        Informedica.GenForm.Lib.DoseRule.get ()
        |> Array.filter (fun dr ->
            dr.DoseLimits
            |> Array.collect _.Products
            |> Array.length >  0 &&
            dr.DoseType <> NoDoseType
        )
//        |> Array.filter (fun p -> p.Generic |> String.contains "/")

    let total1 =
        dsrs
        |> Array.collect (fun dr ->
            dr.DoseLimits
            |> Array.collect _.Products
            |> Array.collect (fun _ ->
                dr.Frequencies
                |> Option.map ValueUnit.getValue
                |> Option.defaultValue [| 1N |]
            )
        )
        |> Array.length

    dsrs
    |> Array.map (fun dr ->
        { dr with
            Shape = ""
            DoseLimits = [||]
        }
    )
    |> Array.distinct
    |> Array.length
    |> fun total2 ->
        printfn $"{total2} -> {total1}"


let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"


let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative
let stopLogger () = OrderLogger.logger.Stop ()



let test pat n =
    let pr =
        pat
        |> PrescriptionRule.get
        |> Array.filter (fun pr ->
            pr.DoseRule.DoseLimits
            |> Array.forall DoseLimit.hasProducts
        )
        |> Array.item n

    pr
    |> Api.evaluate { Log = ignore }
    |> Array.map (function
        | Ok (ord, pr) ->
            let ns =
                pr.DoseRule.DoseLimits
                |> Array.map (fun dl -> dl.DoseLimitTarget |> DoseLimit.doseLimitTargetToString)
            let o =
                ord
                |> Order.Print.printOrderToString true ns
            let p =
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.DoseType |> DoseType.toDescription} {pr.DoseRule.Indication}"
            Ok (pat, p, o)
        | Error (ord, pr, m) ->
            let o =
                ord
                |> Order.toString
                |> String.concat "\n"
            let p =
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

            Error ($"%A{m}", p, o)
    )


let getN pat =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr ->
        pr.DoseRule.DoseLimits
        |> Array.forall DoseLimit.hasProducts
    )
    |> Array.length


let run n pat =
    for i in [0..n-1] do
        try
            i
            |> test pat
            |> Array.map (function
                | Ok (pat, ind, (prs, prep, adm)) ->
                    [
                        ""
                        $"{i}"
                        $"Patient: {pat |> Patient.toString}"
                        $"Indicatie: {ind}"
                        $"Voorschrift: {prs}"
                        if prep |> String.notEmpty then $"Bereiding: {prep}"
                        $"Toediening: {adm}"
                        ""
                    ]
                    |> String.concat "\n"
                | Error (_, p, _) -> $"\n{i}.Fail: {p}\n"
            )
            |> String.concat "\n"

        with
        | _ ->
            let pr =
                pat
                |> PrescriptionRule.get
                |> Array.filter (fun pr ->
                    pr.DoseRule.DoseLimits
                    |> Array.forall DoseLimit.hasProducts
                )
                |> Array.item i
                |> fun pr ->
                    $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

            $"\n{i}. could not calculate: {pr}\n"
        |>  File.appendTextToFile path.Value



let getRule i pat =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr ->
        pr.DoseRule.DoseLimits
        |> Array.forall DoseLimit.hasProducts
    )
    |> Array.item i


let createScenarios () =
    [
        Patient.premature
        Patient.newBorn
        Patient.infant
        Patient.toddler
        Patient.child
        Patient.teenager
        Patient.adult
    ]
    // |> List.skip 4
    // |> List.take 1
    |> List.iter (fun pat ->
        let n = getN pat
        printfn $"=== Running pat: {pat |> Patient.toString}: {n} ==="

        pat
        |> run n
    )



let setAge a (pat : Patient) =
    { pat with Age = Some a }


let pat =
    Patient.infant
    |> setAge
        (Units.Time.year |> ValueUnit.singleWithValue 16N)


let pr =
    Patient.infant
    |> fun p ->
        { p with
            Locations =  [Locations.CVL]
            Department = Some "ICK"
            Age =
                Units.Time.year
                |> ValueUnit.singleWithValue 1N
                |> Some
            Weight =
              Units.Weight.kiloGram
              |> ValueUnit.singleWithValue (10N)
              |> Some
        }
    //|> Api.scenarioResult |> Api.filter
    //|> fun p -> { p with VenousAccess = CVL; AgeInDays = Some 0N }
    |> PrescriptionRule.get
    |> Array.item 0 //|> Api.evaluate (OrderLogger.logger.Logger)


Patient.infant
|> fun p ->
    { p with
        Locations =  []
        Department = Some "ICK"
    }
//|> Api.scenarioResult |> Api.filter
//|> fun p -> { p with VenousAccess = CVL; AgeInDays = Some 0N }
|> PrescriptionRule.get
|> Array.item 0 //|> Api.evaluate (OrderLogger.logger.Logger)
//|> fun pr -> pr |> DrugOrder.createDrugOrder None  //|> printfn "%A"
|> fun pr -> pr |> DrugOrder.createDrugOrder (pr.SolutionRules[0] |> Some)  //|> printfn "%A"
|> DrugOrder.toOrderDto
|> Order.Dto.fromDto //|> Order.toString |> List.iter (printfn "%s")
|> Order.Dto.toDto
|> Order.Dto.fromDto
|> Order.applyConstraints //|> Order.toString |> List.iter (printfn "%s")
|> fun ord ->
    printfn "constraints applied"
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"
    ord

|> Order.solveMinMax true OrderLogger.logger.Logger
|> Result.map (fun ord ->
    printfn "solve min max"
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%A"

    ord
    |> Order.Print.printOrderToMd true [| |]
    //|> String.concat "\n"
    |> printfn "%A"
    ord
) //|> ignore
|> Result.bind (Api.increaseIncrements OrderLogger.logger.Logger)
|> function
| Error (ord, msgs) ->
    printfn "oeps error"
    // printfn $"{msgs |> List.map string}"
    // ord
    // |> Order.toString
    // |> String.concat "\n"
    // |> printfn "%s"

| Ok ord  ->
//    ord.Orderable.OrderableQuantity
//    |> printfn "%A"
    printfn "increased increment"
    ord
    //|> Order.Markdown.printPrescription [|"insuline aspart"|]
    //|> fun (prs, prep, adm) -> printfn $"{prs}"
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"

    ord
    |> Order.solveOrder true OrderLogger.noLogger
    |> function
        | Error (_, msgs) ->
            printfn "oeps error"
            printfn $"{msgs |> List.map string}"
            // ord
            // |> Order.toString
            // |> String.concat "\n"
            // |> printfn "%s"
        | Ok ord  ->
            //|> Order.Print.printOrderToMd true [||]
            //|> fun (prs, prep, adm) ->
            //    printfn $"preparation:\n{pres}"
            ord
            |> Order.toString
            |> String.concat "\n"
            |> printfn "%s"

            ord
            |> Order.solveMinMax false OrderLogger.logger.Logger
            |> function
            | Error (_, msgs) ->
                printfn "oeps error"
                printfn $"{msgs |> List.map string}"
                // ord
                // |> Order.toString
                // |> String.concat "\n"
                // |> printfn "%s"
            | Ok ord  ->
                //|> Order.Print.printOrderToMd true [||]
                //|> fun (prs, prep, adm) ->
                //    printfn $"preparation:\n{pres}"
                ord
                |> Order.toString
                |> String.concat "\n"
                |> printfn "%s"

                ord
                |> Order.minIncrMaxToValues OrderLogger.logger.Logger
                |> fun ord ->
                    ord
                    |> Order.toString
                    |> String.concat "\n"
                    |> printfn "%s"
                ord
                |> Order.Dto.toDto
                |> Order.Dto.fromDto
                |> Order.solveMinMax false OrderLogger.logger.Logger
                |> function
                | Error (_, msgs) ->
                    printfn "oeps error"
                    printfn $"{msgs |> List.map string}"
                    // ord
                    // |> Order.toString
                    // |> String.concat "\n"
                    // |> printfn "%s"
                | Ok ord  ->
                    //|> Order.Print.printOrderToMd true [||]
                    //|> fun (prs, prep, adm) ->
                    //    printfn $"preparation:\n{pres}"
                    ord
                    |> Order.toString
                    |> String.concat "\n"
                    |> printfn "%s"



test Patient.premature 111
|> Array.iter (function
    | Ok (pat, ind, (prs, prep, adm)) ->
        [
            $"Patient: {pat |> Patient.toString}"
            $"Indicatie: {ind}"
            $"Voorschrift: {prs}"
            if prep |> String.notEmpty then $"Bereiding: {prep}"
            $"Toediening: {adm}"
        ]
        |> List.iter (printfn "%s")
    | Error _ -> ()
)


try
    let ord =
        Patient.child
        |> getRule 703
        |> DrugOrder.createDrugOrder None
        |> DrugOrder.toOrderDto
        |> Order.Dto.fromDto
        |> Order.applyConstraints

    let mapping =
        match ord.Prescription with
        | Continuous -> Order.Mapping.continuous
        | Discontinuous _ -> Order.Mapping.discontinuous
        | Timed _ -> Order.Mapping.timed
        | Once -> failwith "todo"
        | OnceTimed time -> failwith "todo"
        |> Order.Mapping.getEquations
        |> Order.Mapping.getEqsMapping ord

    printfn $"{mapping}"

    let oEqs =
        ord
        |> Order.mapToOrderEquations mapping

    oEqs
    |> Solver.mapToSolverEqs



with
| :? Informedica.GenSolver.Lib.Exceptions.SolverException as e ->
    printfn $"{e.Data0}"
    raise e



let testDto =
    Patient.adult
    |> PrescriptionRule.get
    |> Array.filter (fun pr ->
        pr.DoseRule.DoseLimits
        |> Array.forall DoseLimit.hasProducts
    )
    |> Array.filter (fun pr ->
        pr.DoseRule.Generic = "trimethoprim/sulfamethoxazol" &&
        pr.DoseRule.Route = "iv" //&&
//        pr.DoseRule.Indication |> String.startsWith "juveniele"
    )
    |> Array.item 0 //|> Api.evaluate (OrderLogger.logger.Logger)
    |> fun pr -> pr |> DrugOrder.createDrugOrder None //(pr.SolutionRules[0] |> Some)  //|> printfn "%A"
    |> DrugOrder.toOrderDto


module ValueUnit = Informedica.GenUnits.Lib.ValueUnit
testDto.Orderable.Components[0].Items[0].ComponentConcentration.Constraints.ValsOpt.Value.Value
|> fun s ->
    $"1 {s}"
    |> ValueUnit.fromString


let vancoDoseRules =
    Informedica.GenForm.Lib.DoseRule.get ()
    |> DoseRule.filter
    //    Filter.filter
       { Filter.doseFilter with Patient = Patient.child }
    |> Array.filter (fun dr ->
        dr.Generic = "vancomycine" &&
        dr.Route = "iv"
    )
    |> Array.skip 2
    |> Array.take 1


vancoDoseRules
//Informedica.GenForm.Lib.DoseRule.get ()
|> DoseRule.filter
    { Filter.doseFilter with
        Patient =
            Patient.child
            |> Patient.calcPMAge
    }
|> Array.map (fun dr -> dr |> DoseRule.reconstitute Patient.child.Department [CVL])
|> Array.filter (fun dr ->
    dr.Generic = "vancomycine" &&
    dr.Route = "iv" &&
    dr.DoseLimits |> Array.forall DoseRule.DoseLimit.hasProducts
)


Patient.child
|> PrescriptionRule.get
|> Array.filter (fun dr ->
    dr.DoseRule.Generic = "vancomycine" &&
    dr.DoseRule.Route = "iv"
)


let vuToDto = Option.bind (ValueUnit.Dto.toDto false "English")
let dto =
    Some
       (ValueUnit
          ([|1N|],
           CombiUnit (Mass (MilliGram 1N), OpPer, Mass (Gram 1N))))
    |> vuToDto

dto.Value
|> ValueUnit.Dto.fromDto


Patient.premature
|> fun p ->
    { p with
        Locations = [CVL]
        Department = Some "ICK"
        Age =
            Units.Time.year
            |> ValueUnit.singleWithValue 0N
            |> Some
        Weight =
          Units.Weight.gram
          |> ValueUnit.singleWithValue (1200N)
          |> Some

    }
|> fun p ->
    let filter =
        { Filter.solutionFilter "adrenaline" with
            Age = p.Age
            Weight = p.Weight
            Shape = Some ""
        }

    SolutionRule.get ()
    |> SolutionRule.filter filter


let gen = "acetylcysteine"
Patient.teenager
|> fun p ->
    { p with
        Locations = []
        Department = Some "ICK"
        (*
        Weight =
          Units.Weight.gram
          |> ValueUnit.singleWithValue (40000N)
          |> Some
        *)
    }
//|> Api.scenarioResult |> Api.filter
//|> fun p -> { p with VenousAccess = CVL; AgeInDays = Some 0N }
|> PrescriptionRule.get
|> Array.filter (fun pr -> pr.DoseRule.Generic = gen)
|> Array.item 0 //|> Api.evaluate (OrderLogger.logger.Logger)
//|> fun pr -> pr |> DrugOrder.createDrugOrder None  //|> printfn "%A"
|> fun pr -> pr |> DrugOrder.createDrugOrder (pr.SolutionRules[0] |> Some)  //|> printfn "%A"
|> DrugOrder.toOrderDto
|> Order.Dto.fromDto //|> Order.toString |> List.iter (printfn "%s")
|> Order.Dto.toDto
|> Order.Dto.fromDto
|> Order.applyConstraints //|> Order.toString |> List.iter (printfn "%s")
|> fun ord ->
    printfn $"constraints applied to: {ord.Orderable.Name |> Name.toString}"

    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%A"

    ord
    |> Order.Print.printOrderToTableFormat true true (gen |> String.splitAt '/')
    |> printfn "%A"
    ord

|> Order.solveMinMax true OrderLogger.logger.Logger
|> Result.map (fun ord ->
    printfn "solve min max"
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%A"

    ord
    |> Order.Print.printOrderToTableFormat true true (gen |> String.splitAt '/')
    |> printfn "%A"

    ord
    |> Order.Print.printOrderToMd true (gen |> String.splitAt '/')
    //|> String.concat "\n"
    |> printfn "%A"
    ord
) |> ignore

