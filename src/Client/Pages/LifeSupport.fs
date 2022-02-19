namespace Pages


module LifeSupport =

    open Feliz
    open Feliz.UseElmish
    open Elmish
    open Feliz.MaterialUI
    open Shared
    open Global
    open Types
    open Views


    type State = { Dialog: string list }


    type Msg =
        | ChangePatient of Patient option
        | RowClick of int * string list
        | CloseDialog


    let init = { Dialog = [] }, Cmd.none


    let update updatePatient (msg: Msg) state =
        match msg with
        | ChangePatient p -> state, Cmd.ofSub (fun _ -> p |> updatePatient)
        | RowClick (i, xs) ->
            Utils.Logging.log "rowclick:" i
            { state with Dialog = xs }, Cmd.none
        | CloseDialog -> { state with Dialog = [] }, Cmd.none


    let useStyles =
        Styles.makeStyles (fun styles theme ->
            {|
                container =
                    styles.create [
                        style.boxSizing.borderBox
                    ]
                mainview =
                    styles.create [
                        style.display.flex
                        style.overflow.hidden
                        style.flexDirection.column
                        style.marginLeft 20
                        style.marginRight 20
                    ]
                patientpanel =
                    styles.create [
                        style.marginTop 70
                        style.top 0
                    ]
                patientdetails =
                    styles.create [
                        style.display.flex
                        style.flexDirection.row
                    ]
            |}
        )


    [<ReactComponent>]
    let View
        (input: {| patient: Patient option
                   updatePatient: Patient option -> unit |})
        =
        let state, dispatch =
            React.useElmish (init, update input.updatePatient, [||])

        let classes_ = useStyles ()

        let patientView =
            Html.div [
                prop.id "patient-view"
                prop.style [ style.marginBottom 20 ]
                prop.children [
                    Views.Patient.render
                        input.patient
                        (ChangePatient >> dispatch)
                ]
            ]

        let emergencyList =
            Html.div [
                prop.id "emergency-list"
                prop.style [
                    style.overflowY.scroll
                    style.display.flex
                    style.top 0
                    //                    style.height
                    style.custom ("height", "100vh")
                    style.marginBottom 150
                ]
                prop.children [
                    Html.div [
                        // prop.className classes.patientdetails
                        prop.style [ style.flexGrow 1 ]
                        prop.children [
                            match input.patient with
                            | None ->
                                Html.div [
                                    prop.style [ style.paddingTop 20 ]
                                    prop.children [
                                        Utils.Typography.h3 "Noodlijst"
                                        Utils.Typography.h5
                                            "Wordt getoond na invoer van patient gegevens"
                                    ]
                                ]
                            | Some p ->
                                let a = p |> Patient.getAgeInYears
                                let w = p |> Patient.getWeight

                                EmergencyList.render a w (RowClick >> dispatch)
                        ]
                    ]
                ]
            ]

        let dialog =
            let content =
                state.Dialog
                |> function
                    | [ ind; int; calc; prep; adv ] ->
                        [ ind; int; calc; prep; adv ]
                        |> List.zip [
                            "indicatie"
                            "interventie"
                            "berekend"
                            "toediening"
                            "advies"
                           ]
                        |> List.collect (fun (s1, s2) ->
                            if s2 = "" then
                                []
                            else
                                [
                                    Mui.listItem [
                                        Mui.listItemText [
                                            listItemText.primary s1
                                            if s1 = "interventie"
                                               || s1 = "toediening" then
                                                listItemText.secondary (
                                                    $"**{s2}**"
                                                    |> Components.Markdown.render
                                                )
                                            else
                                                listItemText.secondary s2
                                        ]
                                    ]
                                    Mui.divider []
                                ]
                        )
                        |> Mui.list
                    | _ ->
                        [
                            "No valid content" |> Components.Markdown.render
                        ]
                        |> React.fragment

            Mui.dialog [
                dialog.open' (state.Dialog |> List.isEmpty |> not)
                dialog.children [
                    Mui.dialogTitle "Details"
                    Mui.dialogContent [
                        prop.style [ style.padding 40 ]
                        prop.children content
                    ]
                    Mui.dialogActions [
                        Mui.button [
                            prop.onClick (fun _ -> CloseDialog |> dispatch)
                            prop.text "OK"
                        ]
                    ]
                ]
                dialog.onClose (fun _ -> CloseDialog |> dispatch)
            ]

        Html.div [
            prop.id "lifesupport-div"
            prop.style [
                style.marginTop 80
                style.display.flex
                style.flexDirection.column
                style.overflowY.hidden
                style.custom ("height", "100vh")
            ]
            prop.children [
                patientView
                emergencyList
                dialog
            ]
        ]


    let render patient updatePatient =
        View(
            {|
                patient = patient
                updatePatient = updatePatient
            |}
        )