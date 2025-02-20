namespace Views

open System
open Fable.Core
open Fable.React
open Feliz
open Browser.Types
open Shared.Types


module Order =


    module private Elmish =

        open Feliz
        open Feliz.UseElmish
        open Elmish
        open Shared
        open Utils
        open FSharp.Core


        type State = {
            SelectedSubstance : string option
            Order : Deferred<Order option>
        }


        type Msg =
            | ChangeSubstance of string option
            | ChangeFrequency of string option
            | ChangeTime of string option
            | ChangeSubstanceDoseQuantity of string option
            | ChangeSubstancePerTime of string option
            | ChangeSubstancePerTimeAdjust of string option
            | ChangeSubstanceRate of string option
            | ChangeSubstanceRateAdjust of string option
            | ChangeSubstanceComponentConcentration of string option
            | ChangeOrderableDoseQuantity of string option
            | ChangeOrderableDoseRate of string option
            | UpdateOrder of Order


        let init (ord: Deferred<(bool * string option * Order ) option>) =
            let SelectedSubstance =

                match ord with
                | Resolved (Some (_, subst, ord)) ->
                    ord.Orderable.Components
                    |> Array.tryHead
                    |> Option.bind (fun c ->
                        if c.Items |> Array.isEmpty then None
                        else
                            // only use substances that are not additional
                            let substs =
                                c.Items
                                |> Array.filter (_.IsAdditional >> not)

                            substs
                            |> Array.tryFind (fun i -> i.Name |> Some = subst)
                            |> Option.map _.Name
                            |> Option.defaultValue (substs[0].Name)
                            |> Some
                    )
                | _ -> None

            {
                SelectedSubstance = SelectedSubstance
                Order =
                    ord
                    |> Deferred.map (fun opt -> opt |> Option.map (fun (_, _, o) -> o))
            }
            , Cmd.none


        let update
            loadOrder
            (msg: Msg)
            (state : State) : State * Cmd<Msg>
            =

            let setVu s (vu : Shared.Types.ValueUnit option) =
                match vu with
                | Some vu ->
                    { vu with
                        Value =
                            vu.Value
                            |> Array.tryFind (fun (v, _) ->
                                let b = v = (s |> Option.defaultValue "")
                                if not b then Logging.warning "couldn't find" s
                                b
                            )
                            |> Option.map Array.singleton
                            |> Option.defaultValue vu.Value
                    } |> Some
                | None -> None

            match msg with

            | UpdateOrder ord ->
                (state.SelectedSubstance, ord) |> loadOrder

                { state with
                    Order = InProgress
                }
                , Cmd.none

            | ChangeSubstance s ->
                match s with
                | None -> state, Cmd.none
                | Some _ -> { state with SelectedSubstance = s }, Cmd.none

            | ChangeFrequency s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Prescription =
                                { ord.Prescription with
                                    Frequency =
                                        { ord.Prescription.Frequency with
                                            Variable =
                                                { ord.Prescription.Frequency.Variable with
                                                    Vals = ord.Prescription.Frequency.Variable.Vals |> setVu s
                                                }
                                        }
                                }
                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeTime s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Prescription =
                                { ord.Prescription with
                                    Time =
                                        { ord.Prescription.Time with
                                            Variable =
                                                { ord.Prescription.Time.Variable with
                                                    Vals = ord.Prescription.Time.Variable.Vals |> setVu s
                                                }
                                        }
                                }
                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeSubstanceDoseQuantity s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Components =
                                        ord.Orderable.Components
                                        |> Array.mapi (fun i comp ->
                                            if i > 0 then comp
                                            else
                                                { comp with
                                                    Items =
                                                        comp.Items
                                                        |> Array.map (fun item ->
                                                            match state.SelectedSubstance with
                                                            | Some subst when subst = item.Name ->
                                                                { item with
                                                                    Dose =
                                                                        { item.Dose with
                                                                            Quantity =
                                                                                { item.Dose.Quantity with
                                                                                    Variable =
                                                                                        { item.Dose.Quantity.Variable with
                                                                                            Vals = item.Dose.Quantity.Variable.Vals |> setVu s
                                                                                        }
                                                                                }

                                                                        }
                                                                }
                                                            | _ -> item
                                                        )
                                                }
                                        )
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeSubstancePerTime s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Components =
                                        ord.Orderable.Components
                                        |> Array.mapi (fun i comp ->
                                            if i > 0 then comp
                                            else
                                                { comp with
                                                    Items =
                                                        comp.Items
                                                        |> Array.map (fun item ->
                                                            match state.SelectedSubstance with
                                                            | Some subst when subst = item.Name ->
                                                                { item with
                                                                    Dose =
                                                                        { item.Dose with
                                                                            PerTime =
                                                                                { item.Dose.PerTime with
                                                                                    Variable =
                                                                                        { item.Dose.PerTime.Variable with
                                                                                            Vals = item.Dose.PerTime.Variable.Vals |> setVu s
                                                                                        }
                                                                                }

                                                                        }
                                                                }
                                                            | _ -> item
                                                        )
                                                }
                                        )
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeSubstancePerTimeAdjust s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Components =
                                        ord.Orderable.Components
                                        |> Array.mapi (fun i comp ->
                                            if i > 0 then comp
                                            else
                                                { comp with
                                                    Items =
                                                        comp.Items
                                                        |> Array.map (fun item ->
                                                            match state.SelectedSubstance with
                                                            | Some subst when subst = item.Name ->
                                                                { item with
                                                                    Dose =
                                                                        { item.Dose with
                                                                            PerTimeAdjust =
                                                                                { item.Dose.PerTimeAdjust with
                                                                                    Variable =
                                                                                        { item.Dose.PerTimeAdjust.Variable with
                                                                                            Vals = item.Dose.PerTimeAdjust.Variable.Vals |> setVu s
                                                                                        }
                                                                                }

                                                                        }
                                                                }
                                                            | _ -> item
                                                        )
                                                }
                                        )
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeSubstanceRate s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Components =
                                        ord.Orderable.Components
                                        |> Array.mapi (fun i comp ->
                                            if i > 0 then comp
                                            else
                                                { comp with
                                                    Items =
                                                        comp.Items
                                                        |> Array.map (fun item ->
                                                            match state.SelectedSubstance with
                                                            | Some subst when subst = item.Name ->
                                                                { item with
                                                                    Dose =
                                                                        { item.Dose with
                                                                            Rate =
                                                                                { item.Dose.Rate with
                                                                                    Variable =
                                                                                        { item.Dose.Rate.Variable with
                                                                                            Vals = item.Dose.Rate.Variable.Vals |> setVu s
                                                                                        }
                                                                                }

                                                                        }
                                                                }
                                                            | _ -> item
                                                        )
                                                }
                                        )
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeSubstanceRateAdjust s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Components =
                                        ord.Orderable.Components
                                        |> Array.mapi (fun i comp ->
                                            if i > 0 then comp
                                            else
                                                { comp with
                                                    Items =
                                                        comp.Items
                                                        |> Array.map (fun item ->
                                                            match state.SelectedSubstance with
                                                            | Some subst when subst = item.Name ->
                                                                { item with
                                                                    Dose =
                                                                        { item.Dose with
                                                                            RateAdjust =
                                                                                { item.Dose.RateAdjust with
                                                                                    Variable =
                                                                                        { item.Dose.RateAdjust.Variable with
                                                                                            Vals = item.Dose.RateAdjust.Variable.Vals |> setVu s
                                                                                        }
                                                                                }

                                                                        }
                                                                }
                                                            | _ -> item
                                                        )
                                                }
                                        )
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeSubstanceComponentConcentration s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Components =
                                        ord.Orderable.Components
                                        |> Array.mapi (fun i comp ->
                                            if i > 0 then comp
                                            else
                                                { comp with
                                                    Items =
                                                        comp.Items
                                                        |> Array.map (fun item ->
                                                            match state.SelectedSubstance with
                                                            | Some subst when subst = item.Name ->
                                                                { item with
                                                                    ComponentConcentration =
                                                                        { item.ComponentConcentration with
                                                                            Variable =
                                                                                { item.ComponentConcentration.Variable with
                                                                                    Vals = item.ComponentConcentration.Variable.Vals |> setVu s
                                                                                }

                                                                        }
                                                                }
                                                            | _ -> item
                                                        )
                                                }
                                        )
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeOrderableDoseQuantity s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Dose =
                                        { ord.Orderable.Dose with
                                            Quantity =
                                                { ord.Orderable.Dose.Quantity with
                                                    Variable =
                                                        { ord.Orderable.Dose.Quantity.Variable with
                                                            Vals = ord.Orderable.Dose.Quantity.Variable.Vals |> setVu s
                                                        }
                                                }

                                        }
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

            | ChangeOrderableDoseRate s ->
                match state.Order with
                | Resolved (Some ord) ->
                    let msg =
                        { ord with
                            Orderable =
                                { ord.Orderable with
                                    Dose =
                                        { ord.Orderable.Dose with
                                            Rate =
                                                { ord.Orderable.Dose.Rate with
                                                    Variable =
                                                        { ord.Orderable.Dose.Rate.Variable with
                                                            Vals = ord.Orderable.Dose.Rate.Variable.Vals |> setVu s
                                                        }
                                                }

                                        }
                                }

                        }
                        |> UpdateOrder
                    { state with Order = HasNotStartedYet}, Cmd.ofMsg msg
                | _ -> state, Cmd.none

        let showOrderName (ord : Deferred<Order option>) =
            match ord with
            | Resolved(Some ord) ->
                $"{ord.Orderable.Name} {ord.Orderable.Components[0].Shape}"
            | Resolved None -> "order kan niet worden berekend"
            | _ -> "order is loading ..."


    open Elmish
    open Shared


    [<JSX.Component>]
    let View (props:
        {|
            order: Deferred<(bool * string option * Order) option>
            loadOrder: (string option * Order) -> unit
            updateScenarioOrder : unit -> unit
            closeOrder : unit -> unit
            localizationTerms : Deferred<string [] []>

        |}) =

        let context = React.useContext Global.context
        let lang = context.Localization

        let getTerm defVal term =
            props.localizationTerms
            |> Deferred.map (fun terms ->
                Localization.getTerm terms lang term
                |> Option.defaultValue defVal
            )
            |> Deferred.defaultValue defVal

        let state, dispatch =
            React.useElmish (
                init props.order,
                update props.loadOrder,
                [| box props.order; box props.loadOrder |]
            )

        let itms =
            match state.Order with
            | Resolved (Some ord) ->
                ord.Orderable.Components
                // only use the main component for dosing
                |> Array.tryHead
                |> Option.map (fun c ->
                    // filter out additional items, they are not used for dosing
                    c.Items
                    |> Array.filter (_.IsAdditional >> not)
                )
                |> Option.defaultValue [||]
            | _ -> [||]

        let substIndx =
            itms
            |> Array.tryFindIndex (fun i ->
                state.SelectedSubstance
                |> Option.map ((=) i.Name)
                |> Option.defaultValue false
            )
            |> function
            | None -> Some 0
            | Some i -> Some i

        let select isLoading lbl selected dispatch xs =
            if xs |> Array.isEmpty then
                JSX.jsx $"<></>"
            else
                Components.SimpleSelect.View({|
                    updateSelected = dispatch
                    label = lbl
                    selected =
                        if xs |> Array.length = 1 then xs[0] |> fst |> Some
                        else selected
                    values = xs
                    isLoading = isLoading
                |})

        let progress =
            match props.order with
            | Resolved _ -> JSX.jsx $"<></>"
            | _ ->
                JSX.jsx
                    $"""
                import CircularProgress from '@mui/material/CircularProgress';

                <Box sx={ {| mt = 5; display = "flex"; p = 20 |} }>
                    <CircularProgress />
                </Box>
                """

        let fixPrecision n d =
            (d |> float)
            |> Utils.Math.fixPrecision n
            |> string

        let onClickOk =
            fun () ->
                props.updateScenarioOrder ()
                props.closeOrder ()

        let content =
            JSX.jsx
                $"""
            import CardHeader from '@mui/material/CardHeader';
            import CardContent from '@mui/material/CardContent';
            import Typography from '@mui/material/Typography';
            import Stack from '@mui/material/Stack';
            import Paper from '@mui/material/Paper';

            <div>

            <CardHeader
                title={props.order |> Deferred.map (Option.map (fun (_, _, o) -> o)) |> showOrderName}
                titleTypographyProps={ {| variant = "h6" |} }
            ></CardHeader>
            <CardContent>
                <Stack direction={"column"} spacing={3} >
                    {
                        match props.order with
                        | Resolved (Some (_, _, o)) ->
                            if o.Orderable.Components |> Array.isEmpty then JSX.jsx $"<></>"
                            else
                                itms
                                |> Array.map _.Name
                                |> Array.map (fun s -> s, s)
                                |> select false "Stoffen" state.SelectedSubstance (ChangeSubstance >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match props.order with
                        | Resolved (Some (_, _, o)) ->
                            o.Prescription.Frequency.Variable.Vals
                            |> Option.map (fun v -> v.Value |> Array.map (fun (s, d) -> s, $"{d |> string} {v.Unit}"))
                            |> Option.defaultValue [||]
                            |> select false (Terms.``Order Frequency`` |> getTerm "Frequentie") None (ChangeFrequency >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match substIndx, props.order with
                        | Some i, Resolved (Some (_, _, o)) when itms |> Array.length > 0->
                            let label, vals =
                                itms[i].Dose.Quantity.Variable.Vals
                                |> Option.map (fun v ->
                                    (Terms.``Continuous Medication Dose``
                                    |> getTerm "Keer Dosis"
                                    |> fun s -> $"{s} ({v.Unit})"),
                                    v.Value
                                    |> Array.map (fun (s, d) -> s, $"{d |> fixPrecision 3} {v.Unit}")
                                    |> Array.distinctBy snd
                                )
                                |> Option.defaultValue ("", [||])

                            vals
                            |> select false label None (ChangeSubstanceDoseQuantity >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match substIndx, props.order with
                        | Some i, Resolved (Some (b, _, o)) when o.Prescription.IsContinuous |> not &&
                                                                 itms |> Array.length > 0 ->
                            let dispatch = if b then ChangeSubstancePerTimeAdjust >> dispatch else ChangeSubstancePerTime >> dispatch
                            let label, vals =
                                if b then
                                    itms[i].Dose.PerTimeAdjust.Variable.Vals
                                else
                                    itms[i].Dose.PerTime.Variable.Vals
                                |> Option.map (fun v ->
                                    (Terms.``Order Adjusted dose``
                                    |> getTerm "Dosering"
                                    |> fun s -> $"{s} ({v.Unit})"),
                                    v.Value
                                    |> Array.map (fun (s, d) -> s, $"{d |> fixPrecision 3} {v.Unit}")
                                    |> Array.distinctBy snd
                                )
                                |> Option.defaultValue ("", [||])

                            vals
                            |> select false label None dispatch
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match substIndx, props.order with
                        | Some i, Resolved (Some (b,_,  o)) when o.Prescription.IsContinuous &&
                                                                 itms |> Array.length > 0 ->
                            let dispatch = if b then ChangeSubstanceRateAdjust >> dispatch else ChangeSubstanceRate >> dispatch
                            if b then
                                itms[i].Dose.RateAdjust.Variable.Vals
                            else
                                itms[i].Dose.Rate.Variable.Vals
                            |> Option.map (fun v ->
                                v.Value
                                |> Array.map (fun (s, d) -> s, $"{d |> fixPrecision 3} {v.Unit}")
                                |> Array.distinctBy snd
                            )
                            |> Option.defaultValue [||]
                            |> select false (Terms.``Order Adjusted dose`` |> getTerm "Dosering") None dispatch
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match props.order with
                        | Resolved (Some (_, _, o)) ->
                            o.Orderable.Dose.Quantity.Variable.Vals
                            |> Option.map (fun v -> v.Value |> Array.map (fun (s, d) -> s, $"{d |> fixPrecision 3} {v.Unit}"))
                            |> Option.defaultValue [||]
                            |> select false (Terms.``Order Quantity`` |> getTerm "Hoeveelheid") None (ChangeOrderableDoseQuantity >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match substIndx, props.order with
                        | Some i, Resolved (Some (_, _, _)) when itms |> Array.length > 0 ->
                            itms[i].ComponentConcentration.Variable.Vals
                            |> Option.map (fun v -> v.Value |> Array.map (fun (s, d) -> s, $"{d |> fixPrecision 3} {v.Unit}"))
                            |> Option.defaultValue [||]
                            |> select false (Terms.``Order Concentration`` |> getTerm "Sterkte") None (ChangeSubstanceComponentConcentration >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match props.order with
                        | Resolved (Some (_, _, o)) ->
                            o.Orderable.Dose.Rate.Variable.Vals
                            |> Option.map (fun v -> v.Value |> Array.map (fun (s, d) -> s, $"{d |> string} {v.Unit}"))
                            |> Option.defaultValue [||]
                            |> select false (Terms.``Order Drip rate`` |> getTerm "Pompsnelheid") None (ChangeOrderableDoseRate >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                    {
                        match props.order with
                        | Resolved (Some (_, _, o))  ->
                            o.Prescription.Time.Variable.Vals
                            |> Option.map (fun v -> v.Value |> Array.map (fun (s, d) -> s, $"{d |> fixPrecision 2} {v.Unit}"))
                            |> Option.defaultValue [||]
                            |> select false (Terms.``Order Administration time`` |> getTerm "Inloop tijd") None (ChangeTime >> dispatch)
                        | _ ->
                            [||]
                            |> select true "" None ignore
                    }
                </Stack>

            </CardContent>
            <CardActions>
                    <Button onClick={onClickOk}>
                        {Terms.``Ok `` |> getTerm "Ok"}
                    </Button>
            </CardActions>
            </div>
            """

        JSX.jsx
            $"""
        import Box from '@mui/material/Box';
        import Card from '@mui/material/Card';
        import CardActions from '@mui/material/CardActions';
        import CardContent from '@mui/material/CardContent';
        import Button from '@mui/material/Button';
        import Typography from '@mui/material/Typography';

        <Card variant="outlined">
                {content}
                {progress}
        </Card>
        """


