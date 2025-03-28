namespace Informedica.ZIndex.Lib


module RuleFinder =


    type MinMax = RuleMinMax



    /// Create a route from a string
    let createRoute = Route.fromString (Route.routeMapping ())


    /// Check if a route exists.
    let eqsRoute = Route.routeExists (Route.routeMapping ())


    /// Check whether a float n is in range of a MinMax.
    let inRange n { Min = min; Max = max } =
        if n |> Option.isNone then true
        else
            let n = n |> Option.get
            match min, max with
            | None, None -> true
            | Some min, None -> n >= min
            | None, Some max -> n <= max
            | Some min, Some max -> n >= min && n <= max


    /// An empty Filter.
    let filter =
        {
            Patient = { Age = None; Weight = None; BSA = None }
            Product =
                { Generic = ""; Shape = ""; Route = "" }
                |> GenericShapeRoute
        }


    /// <summary>
    /// Create a filter.
    /// </summary>
    /// <param name="age">Age</param>
    /// <param name="wght">Weight</param>
    /// <param name="bsa">Body Surface Area</param>
    /// <param name="gpk">Generic Product Id</param>
    /// <param name="gen">Generic name</param>
    /// <param name="shp">Shape</param>
    /// <param name="rte">Route</param>
    let createFilter age wght bsa gpk gen shp rte =
        let pat = { Age = age; Weight = wght; BSA = bsa }
        let prod =
            if gpk |> Option.isSome then
                (gpk |> Option.get, rte)
                |> GPKRoute
            else
                {
                    Generic = gen
                    Shape = shp
                    Route = rte
                } |> GenericShapeRoute
        {
            Patient = pat
            Product = prod
        }


    /// <summary>
    /// Create a Filter for GPK, Route.
    /// </summary>
    /// <param name="gpk">The Generic Product Id</param>
    /// <param name="rte">The Route</param>
    let createGPKRouteFilter gpk rte = createFilter None None None gpk "" "" rte


    /// <summary>
    /// Find all DoseRules for a list of GPKs and a Filter.
    /// </summary>
    /// <param name="gpks">The GPKs</param>
    /// <param name="filter">The Filter</param>
    let find gpks filter =
        let { Filter.Patient = pat; Product = prod }  = filter
        let r =
            match prod with
            | GPKRoute (_, route)   -> route
            | GenericShapeRoute gsr -> gsr.Route
            |> createRoute

        match prod with
        | GPKRoute (gpk, _) -> [| gpk |]
        | GenericShapeRoute gsr ->
            GenPresProduct.filter gsr.Generic gsr.Shape gsr.Route
            |> Array.collect (fun gpp ->
                gpp.GenericProducts
                |> Array.map _.Id
            )
        |> Array.collect (fun gpk ->
            DoseRule.get gpks
            |> Array.filter (fun dr ->
                (dr.CareGroup = DoseRule.Constants.intensive || dr.CareGroup = DoseRule.Constants.all)
                && dr.GenericProduct |> Array.exists (fun gp -> gp.Id = gpk)
                && dr.Routes |> eqsRoute r
                && dr.Age    |> inRange pat.Age
                && dr.Weight |> inRange pat.Weight
                && dr.BSA    |> inRange pat.BSA
            )
            |> Array.distinct
        )


    /// Create a RuleResult.
    let createResult gpp drs ds =
        {
            Product = gpp
            DoseRules = drs
            Doses = ds
        }


    /// Create a FrequencyDose.
    let createFreqDose freq norm abs normKg absKg normM2 absM2 un =
        {
            Freq = freq
            NormDose = norm
            AbsDose = abs
            NormKg = normKg
            AbsKg = absKg
            NormM2 = normM2
            AbsM2 = absM2
            Unit = un
        }


    /// <summary>
    /// Convert a list of DoseRules to a RuleResult.
    /// </summary>
    /// <param name="drs">The DoseRules</param>
    /// <remarks>
    /// If the DoseRules are not for the same GenPresProduct then
    /// None is returned.
    /// </remarks>
    let convertToResult (drs : DoseRule  []) =

        // Get the min max weight if there is one min weight or max weight
        let wghtMinMax (drs : DoseRule []) =

            match drs |> Array.toList with
            | [] -> DoseRule.minmax
            | [h] -> h.Weight
            | h::tail ->
                if tail |> List.forall (fun mm -> mm.Weight = h.Weight) then h.Weight
                else DoseRule.minmax

        // Get the min max weight if there is one min weight or max weight
        let bsaMinMax (drs : DoseRule []) =

            match drs |> Array.toList with
            | [] -> DoseRule.minmax
            | [h] -> h.BSA
            | h::tail ->
                if tail |> List.forall (fun mm -> mm.BSA = h.Weight) then h.BSA
                else DoseRule.minmax

        // Alle dose rules should apply to the same
        // GenPresProduct
        let gpp =
            drs
            |> Array.collect (fun dr ->
                dr.GenericProduct
                |> Array.map _.Id
            )
            |> Array.distinct
            |> Array.collect GenPresProduct.findByGPK
            |> (fun gpps ->
                if gpps |> Array.isEmpty then None
                else
                    gpps
                    |> Array.fold (fun acc gpp ->
                        match acc with
                        | Some gpp' -> if gpp' = gpp then acc else None
                        | None -> None
                    ) (Some gpps[0])
            )

        match gpp with
        | Some gpp' ->
            let multMinMax f n { Min = min; Max = max } =
                let m = f * n

                let mn, mx =
                    match min, max with
                    | None, None           -> (0., 0.)
                    | Some min', None      -> (min' * m, 0.)
                    | None, Some max'      -> (0., max' * m )
                    | Some min', Some max' -> (min' * m, max' * m)

                DoseRule.createMinMax mn mx

            let gpks (dr : DoseRule) =
                dr.GenericProduct
                |> Array.map _.Id
                |> Array.toList
                |> GenericProduct.get


            // Calculate the normal min max dose
            let calcDose get drs =
                drs
                |> Array.collect (fun dr ->
                    dr
                    |> gpks
                    |> Array.map (fun gp ->
                        let n =
                            (gp.Substances
                            |> Array.head).SubstanceQuantity
                        dr |> get |> multMinMax dr.Freq.Frequency n)
                    )
                |> DoseRule.foldMinMax


            // Calculate the normal min max dose
            let norm = calcDose DoseRule.Optics.getNorm

            // Calculate the absolute min max dose
            let abs = calcDose DoseRule.Optics.getAbs

            // Calculate the normal min max dose per kg
            let normKg = calcDose DoseRule.Optics.getNormKg

            // Calculate the absolute min max dose per kg
            let absKg = calcDose DoseRule.Optics.getAbsKg

            // Calculate the normal min max dose per m2
            let normM2 = calcDose DoseRule.Optics.getNormM2

            // Calculate the absolute min max dose per m2
            let absM2 = calcDose DoseRule.Optics.getAbsM2

            let calcNoneAndAdjusted
                (calcAdj   : DoseRule [] -> MinMax)
                (calcNorm  : DoseRule [] -> MinMax)
                (calcPerKg : DoseRule [] -> MinMax) drs =

                let wght  = drs |> calcAdj
                let norm  = drs |> calcNorm
                let perKg = drs |> calcPerKg

                let calc op x1 x2 y =
                    match y with
                    | Some _ -> y
                    | None ->
                        match x1, x2 with
                        | Some x1_, Some x2_ -> (x1_ |> op <| x2_) |> Some
                        | _ -> y

                // Norm.min = PerKg.min * Wght.min
                // Norm.max = PerKg.max * Wght.max
                {
                    Min = norm.Min |> calc (*) perKg.Min wght.Min
                    Max = norm.Max |> calc (*) perKg.Max wght.Max } ,
                // PerKg.min = Norm.min / Wght.max
                // PerKg.max = norm.max / Wght.min
                {
                    Min = perKg.Min |> calc (/) norm.Min wght.Max
                    Max = perKg.Max |> calc (/) norm.Max wght.Min
                }

            let calcNormPerKg = calcNoneAndAdjusted wghtMinMax norm normKg
            let calcAbsPerKg  = calcNoneAndAdjusted wghtMinMax abs  absKg
            let calcNormPerM2 = calcNoneAndAdjusted bsaMinMax  norm normM2
            let calcAbsPerM2  = calcNoneAndAdjusted bsaMinMax  abs  absM2

            let un drs' =
                drs'
                |> Array.fold (fun acc dr ->
                    dr
                    |> gpks
                    |> Array.fold (fun acc' gp ->
                        gp.Substances
                        |> Array.fold (fun acc'' s ->
                            if acc'' = "" then s.SubstanceUnit
                            else
                                if acc'' <> s.SubstanceUnit then "_"
                                else s.SubstanceUnit
                        ) acc'
                    ) acc
                ) ""
                |> (fun u -> if u = "_" then "" else u)

            let freqs =
                drs
                |> Array.map _.Freq
                |> Array.distinct
                |> Array.map (fun fr ->
                    let drs' =
                        drs
                        |> Array.filter (fun dr -> dr.Freq = fr)
                    createFreqDose
                        fr
                        ([| drs' |> calcNormPerKg |> fst; drs' |> calcNormPerM2 |> fst |] |> DoseRule.foldMinMax )
                        ([| drs' |> calcAbsPerKg  |> fst; drs' |> calcAbsPerM2  |> fst |] |> DoseRule.foldMinMax )
                        (drs' |> calcNormPerKg |> snd)
                        (drs' |> calcAbsPerKg  |> snd)
                        (drs' |> calcNormPerM2 |> snd)
                        (drs' |> calcAbsPerM2  |> snd)
                        (drs' |> un)
                )

            createResult gpp' (drs |> Array.map (DoseRule.toString ", ")) freqs
            |> Some

        | None -> None

