namespace Pages


open System
open Fable.Core
open Fable.React
open Feliz
open Browser.Types


open Elmish
open Shared


module GenPres =


    module private Elmish =


        open Global


        type State =
            {
                SideMenuItems: (JSX.Element option * string * bool) []
                SideMenuIsOpen: bool
                Configuration: Configuration Option
                Language: Localization.Locales
            }


        type Msg =
            | SideMenuClick of string
            | ToggleMenu
            | LanguageChange of string
            | UpdateLanguage of Localization.Locales


        let pages =
            [
                LifeSupport
                ContinuousMeds
                Prescribe
                Formulary
                Parenteralia
            ]


        let init page : State * Cmd<Msg> =

            let state =
                {
                    SideMenuItems =
                        pages
                        |> List.toArray
                        |> Array.map (fun p ->
                            let b = p = page
                            match p |> pageToString Localization.Dutch with
                            | s when p = LifeSupport -> (Mui.Icons.FireExtinguisher |> Some), s, b
                            | s when p = ContinuousMeds -> (Mui.Icons.Vaccines |> Some), s, b
                            | s when p = Prescribe -> (Mui.Icons.Message |> Some), s, b
                            | s when p = Formulary -> (Mui.Icons.LocalPharmacy |> Some), s, b
                            | s when p = Parenteralia -> (Mui.Icons.Bloodtype |> Some), s, b
                            | s -> None, s, b
                        )

                    SideMenuIsOpen = false
                    Configuration = None
                    Language = Localization.Dutch
                }

            state, Cmd.none


        let update updatePage (msg: Msg) (state: State) =
            match msg with
            | ToggleMenu ->
                { state with
                    SideMenuIsOpen = not state.SideMenuIsOpen
                },
                Cmd.none

            | SideMenuClick s ->
                let pageToString = Global.pageToString Localization.Dutch

                pages
                |> List.map (fun p -> p |> pageToString, p)
                |> List.tryFind (fst >> ((=) s))
                |> Option.map snd
                |> Option.defaultValue LifeSupport
                |> updatePage

                { state with

                    SideMenuItems =
                        state.SideMenuItems
                        |> Array.map (fun (icon, item, _) ->
                            if item = s then
                                printfn $"{s} true"
                                (icon, item, true)
                            else
                                printfn $"{s} false"
                                (icon, item, false)
                        )
                },
                Cmd.none

            | LanguageChange s ->
                //TODO: doesn't work anymore
                state, Cmd.none

            | UpdateLanguage l -> { state with Language = l }, Cmd.none


    open Elmish



    [<JSX.Component>]
    let View
        (props: {|
            patient: Patient option
            updatePatient: Patient option -> unit
            updatePage: Global.Pages -> unit
            bolusMedication: Deferred<Intervention list>
            continuousMedication: Deferred<Intervention list>
            products: Deferred<Product list>
            scenario: Deferred<ScenarioResult>
            updateScenario : ScenarioResult -> unit
            formulary: Deferred<Formulary>
            updateFormulary : Formulary -> unit
            page : Global.Pages |}) =

        let state, dispatch = React.useElmish (init props.page, update props.updatePage, [| box props.page; box props.updatePage |])

        let notFound =
            JSX.jsx
                $"""
            <React.Fragment>
                <Typography>
                    Nog niet geimplementeerd
                </Typography>
            </React.Fragment>
            """

        JSX.jsx
            $"""
        import {{ ThemeProvider }} from '@mui/material/styles';
        import CssBaseline from '@mui/material/CssBaseline';
        import React from "react";
        import Stack from '@mui/material/Stack';
        import Box from '@mui/material/Box';
        import Container from '@mui/material/Container';
        import Typography from '@mui/material/Typography';

        <React.Fragment>
            <Box>
                {Components.TitleBar.View({|
                    title = $"GenPRES 2023 {props.page |> (Global.pageToString Localization.Dutch)}"
                    toggleSideMenu = fun _ -> ToggleMenu |> dispatch
                |})}
            </Box>
            <React.Fragment>
                {
                    Components.SideMenu.View({|
                        anchor = "left"
                        isOpen = state.SideMenuIsOpen
                        toggle = (fun _ -> ToggleMenu |> dispatch)
                        menuClick = SideMenuClick >> dispatch
                        items =  state.SideMenuItems
                    |})
                }
            </React.Fragment>
            <Container sx={ {| height="87%"; mt= 4 |} } >
                <Stack sx={ {| height="100%" |} }>
                    <Box sx={ {| flexBasis=1 |} } >
                        { Views.Patient.View({| patient = props.patient; updatePatient = props.updatePatient |}) }
                    </Box>
                    <Box sx={ {| maxHeight = "80%"; mt=4; overflowY="auto" |} }>
                        {
                            match props.page with
                            | Global.Pages.LifeSupport ->
                                Views.EmergencyList.View ({| interventions = props.bolusMedication |})
                            | Global.Pages.ContinuousMeds ->
                                Views.ContinuousMeds.View ({| interventions = props.continuousMedication |})
                            | Global.Pages.Prescribe ->
                                Views.Prescribe.View ({| scenarios = props.scenario; updateScenario = props.updateScenario |})
                            | Global.Pages.Formulary ->
                                Views.Formulary.View ({| formulary = props.formulary; updateFormulary = props.updateFormulary |})
                            | _ -> notFound
                        }
                    </Box>
                </Stack>
            </Container>
        </React.Fragment>
        """