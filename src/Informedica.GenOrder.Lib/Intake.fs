namespace Informedica.GenOrder.Lib



module Intake =


    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib
    open Informedica.GenOrder.Lib
    open Informedica.GenSolver.Lib.Variable.Operators


    let isVolume (var : Variable) =
        var
        |> Variable.getUnit
        |> Option.map (fun u ->
            u |> Units.hasGroup Units.Volume.liter
        )
        |> Option.defaultValue false


    let getDose tu pres (dose: Dose) =
        match pres with
        | Timed _
        | Discontinuous _ ->
            dose.PerTime
            |> OrderVariable.PerTime.setTimeUnit tu
            |> OrderVariable.PerTime.toOrdVar
            |> OrderVariable.getVar
        | Continuous ->
            dose.Rate
            |> OrderVariable.Rate.setTimeUnit tu
            |> OrderVariable.Rate.toOrdVar
            |> OrderVariable.getVar
        | Once
        | OnceTimed _ ->
            let var =
                dose.Quantity
                |> OrderVariable.Quantity.toOrdVar
                |> OrderVariable.getVar
            let unt =
                var
                |> Variable.getUnit
                |> Option.map (fun u -> u |> Units.per tu)

            unt
            |> Option.map (fun u ->
                var
                |> Variable.setUnit u
            )
            |> Option.defaultValue var


    let getVolume tu pres (dose: Dose) =
        let ovar = getDose tu pres dose

        if ovar |> isVolume then Some ovar
        else None


    let calc wght tu (ords : Order[]) =
        match wght with
        | None -> [||]
        | Some w ->
            let w =
                Name.create ["wght"]
                |> Variable.empty
                |> fun var ->
                    w
                    |> Variable.ValueRange.ValueSet.create
                    |> ValSet
                    |> Variable.setValueRange var

            [|
                for o in ords do
                    let vol = getVolume tu o.Prescription o.Orderable.Dose
                    if vol.IsSome then "volume", vol.Value

                    for cmp in o.Orderable.Components do
                        for itm in cmp.Items do
                            itm.Name |> Name.toString, getDose tu o.Prescription itm.Dose
            |]
            |> Array.groupBy fst
            |> Array.map (fun (item, xs) ->
                item,
                xs
                |> Array.map snd
                |> Array.reduce (^+)
            )
            |> Array.choose (fun (n, tot) ->
                match tot |> Variable.getUnit with
                | None   -> None
                | Some u ->
                    let u =
                        u
                        |> ValueUnit.getUnits
                        |> List.head
                        |> Units.per Units.Weight.kiloGram
                        |> Units.per tu
                    (n,
                    tot ^/ w
                    |> Variable.setUnit u)
                    |> Some
            )



    let getIntake (wght : Informedica.GenUnits.Lib.ValueUnit option) (dto: Order.Dto.Dto []) : Intake =
        let intake =
            dto
            |> Array.map Order.Dto.fromDto
            |> calc wght Informedica.GenUnits.Lib.Units.Time.day

        let get n =
                intake
                |> Array.tryFind (fst >> String.equalsCapInsens n)
                |> Option.map (fun (_, var) ->
                    var
                    |> Informedica.GenSolver.Lib.Variable.getValueRange
                    |> Informedica.GenSolver.Lib.Variable.ValueRange.toMarkdown 3
                )

        {
            Volume = get "volume"
            Energy = get "energie"
            Protein = get "eiwit"
            Carbohydrate = get "koolhydraat"
            Fat = get "vet"
            Sodium = get "natrium"
            Potassium = get "kalium"
            Chloride = get "chloor"
            Calcium = get "calcium"
            Phosphate = get "fosfaat"
            Magnesium = get "magnesium"
            Iron = get "ijzer"
            VitaminD = get "VitD"
            Ethanol = get "ethanol"
            Propyleenglycol = get "propyleenglycol"
            BenzylAlcohol = get "benzylalcohol"
            BoricAcid = get "boorzuur"
        }