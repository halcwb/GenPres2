
// load demo or product cache
System.Environment.SetEnvironmentVariable("GENPRES_PROD", "1")
System.Environment.SetEnvironmentVariable("GENPRES_URL_ID", "1IZ3sbmrM4W4OuSYELRmCkdxpN9SlBI-5TLSvXWhHVmA")

#load "load.fsx"

#time

open MathNet.Numerics
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib
open Informedica.GenSolver.Lib.Variable.Operators



/// Get all possible prescriptions for a child
let prescrs =
    Patient.teenager
    |> PrescriptionRule.get

Informedica.ZIndex.Lib.GenPresProduct.findByGPK 73377

prescrs |> Array.length


let getPrescr gen shp rte prescrs =
    prescrs
    |> Array.filter (fun prescr ->
        prescr.DoseRule.Generic = gen &&
        (shp |> String.isNullOrWhiteSpace || prescr.DoseRule.Shape = shp) &&
        (rte |> String.isNullOrWhiteSpace || prescr.DoseRule.Route = rte)
    )
    |> Array.item 0
    |> Api.evaluate Logging.ignore
    |> Array.head
    |> function
        | Ok (o, _) -> o
        | Error (o, _, _) -> o



// oral solution for PCM
let pcmOralSolution =
    prescrs
    |> getPrescr "paracetamol" "drank" ""


let amoxyIv =
    prescrs
    |> getPrescr "amoxicilline" "" ""

let adrenInf =
    prescrs
    |> getPrescr "adrenaline" "" "INTRAVENEUS"

let cotrimIv =
    prescrs
    |> getPrescr "trimethoprim/sulfamethoxazol" "" "INTRAVENEUS"

let tpn =
    prescrs
    |> getPrescr "Samenstelling C" "" ""


adrenInf |> Order.toString |> List.iter (printfn "%s")


let w =
    Patient.child.Weight
    |> Option.map (ValueUnit.convertTo Units.Weight.kiloGram)

open Informedica.GenSolver.Lib.Variable.ValueRange.Minimum

[|
    adrenInf
|]
|> Intake.calc w Units.Time.day
|> fun intake ->
    intake[0]
    |> snd
    |> Variable.getValueRange
    |> Variable.ValueRange.getMin
    |> Option.map (fun min ->
        let _, vu = min |> toBoolValueUnit

        let s =
            $"""{vu |> ValueUnit.toDelimitedString 3}"""

        if false then s
        else
            let u =
                vu
                |> ValueUnit.getUnit
                |> Units.toStringDutchShort
                |> String.removeBrackets

            printfn $"{u}"
            s
            |> String.replace $"|{u}|" ""
    )

adrenInf
|> Order.Dto.toDto
|> Api.getIntake w

let shp = ""
let rte = "INTRAVENEUS"
prescrs
|> Array.filter (fun prescr ->
    prescr.DoseRule.Generic = "lorazepam" &&
    (shp |> String.isNullOrWhiteSpace || prescr.DoseRule.Shape = shp) &&
    (rte |> String.isNullOrWhiteSpace || prescr.DoseRule.Route = rte)
)
|> Array.item 0
|> fun r -> DrugOrder.createDrugOrder None r

Informedica.GenForm.Lib.Product.get ()
|> Array.filter (fun p -> p.GPK = "170976")

