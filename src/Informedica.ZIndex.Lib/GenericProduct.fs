namespace Informedica.ZIndex.Lib


module GenericProduct =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    /// Creates a ProductSubstance
    let createSubstance si so sn sq su gi gn gq gu un ia =
        {
            SubstanceId = si
            SortOrder = so
            SubstanceName = sn
            SubstanceQuantity = sq
            SubstanceUnit = su
            GenericId = gi
            GenericName = gn
            GenericQuantity = gq
            GenericUnit = gu
            ShapeUnit = un
            IsAdditional = ia
        }


    /// Create a GenericProduct
    let create id nm lb ac an sh rt ss ps =
        {
            Id = id
            Name = nm
            Label = lb
            ATC = ac
            ATCName = an
            Shape = sh
            Route = rt
            Substances = ss
            PrescriptionProducts = ps
        }


    /// Get the routes for a GenericProduct
    let getRoutes (gp : Zindex.BST711T.BST711T) =
        // try to get the 'enkelvoudige toedieningsweg'
        let rts =
            Zindex.BST052T.records ()
            |> Array.filter (fun prp ->
                prp.MUTKOD <> 1 &&
                prp.GPKODE = gp.GPKODE
            )
            |> Array.collect (fun prp ->
                Zindex.BST031T.records ()
                |> Array.filter (fun hp ->
                    hp.MUTKOD <> 1 &&
                    hp.PRKODE = prp.PRKODE
                )
            )
            |> Array.collect (fun hp ->
                Zindex.BST760T.records ()
                |> Array.filter (fun rt ->
                    rt.MUTKOD <> 1 &&
                    rt.HPKODE = hp.HPKODE
                )
                |> Array.map(_.ENKTDW)
            )
            |> Array.distinct
            |> Array.map (fun rt ->
                Names.getThes rt Names.Route Names.Fifty
            )
            |> Array.filter String.notEmpty

        if rts |> Array.isEmpty |> not then rts
        else
            [| Names.getThes gp.GPKTWG Names.Route Names.Fifty |]
        |> Array.collect (String.split "," >> List.toArray)
        |> Array.map String.trim
        |> Array.filter (String.equalsCapInsens "parenteraal" >> not)
        |> Array.distinct


    /// Get the Substances for a GenericProduct
    let getSubstances un (gp : Zindex.BST711T.BST711T) hpks =
        Zindex.BST715T.records ()
        |> Array.filter (fun gs ->
            gs.GSKODE = gp.GSKODE &&
            gs.MUTKOD <> 1
        )
        |> Array.collect (fun gs ->
            Zindex.BST750T.records ()
            |> Array.filter (fun gn ->
                gn.MUTKOD <> 1 && gs.GNNKPK = gn.GNGNK
            )
            |> Array.map (fun gn -> gs, gn)
        )
        |> Array.collect (fun (gs, gn) ->
            let stam =
                Zindex.BST750T.records ()
                |> Array.find (fun s -> s.GNGNK = gn.GNSTAM)

            let isAdditional = gs.GNMWHS = "H"

            match hpks with
            | _ when hpks |> Array.isEmpty ->
                let un1 = Names.getThes gs.XNMOME Names.GenericUnit Names.Fifty
                createSubstance gn.GNSTAM 1 stam.GNGNAM gs.GNMOMH un1 gn.GNGNK gn.GNGNAM gs.GNMOMH un1 un isAdditional
                |> Array.singleton
            | _  ->
                hpks
                |> Array.collect (fun hpk ->
                    Zindex.BST701T.records ()
                    |> Array.filter (fun ig ->
                        ig.HPKODE = hpk &&
                        ig.GNGNK = gn.GNGNK &&
                        ig.MUTKOD <> 1
                    )
                    |> function
                    | igs when igs |> Array.isEmpty ->
                        Zindex.BST701T.records ()
                        |> Array.filter (fun ig ->
                            ig.HPKODE = hpk &&
                            ig.GNSTAM = gn.GNSTAM &&
                            ig.MUTKOD <> 1
                        )
                    | igs -> igs
                    |> Array.map (fun ig ->
                        let un1 = Names.getThes ig.XNMINE Names.GenericUnit Names.Fifty
                        let un2 = Names.getThes gs.XNMOME Names.GenericUnit Names.Fifty
                        createSubstance ig.GNSTAM ig.GNVOLG stam.GNGNAM ig.GNMINH un1 gn.GNGNK gn.GNGNAM gs.GNMOMH un2 un isAdditional
                    )
                )
        )
        |> Array.distinct
        |> Array.sortBy _.SortOrder


    let private _get gpks =
        Zindex.BST711T.records ()
        |> Array.filter (fun gp ->
            gp.MUTKOD <> 1 &&
            gp.GPKTVR <> 980 && // filter shape <> "NIET VAN TOEPASSING"
            (gpks |> List.isEmpty ||
             gpks
             |> List.exists ((=) gp.GPKODE)))
        |> Array.map (fun gp ->
            let nm = Names.getName gp.GPNMNR Names.Full
            let lb = Names.getName gp.GPNMNR Names.Label

            let an =
                match
                    Zindex.BST801T.records ()
                    |> Array.tryFind (fun atc ->
                        atc.MUTKOD <> 1 &&
                        atc.ATCODE = gp.ATCODE
                    ) with
                | Some atc' -> atc'.ATOMS
                | None      -> ""
            let sh = Names.getThes gp.GPKTVR Names.Shape Names.Fifty
            let rt = getRoutes gp
            let ps = PrescriptionProduct.get gp.GPKODE
            let un = Names.getThes gp.XPEHHV Names.ShapeUnit Names.Fifty

            let ss =
                ps
                |> Array.collect (fun pp ->
                    pp.TradeProducts
                    |> Array.map _.Id
                )
                |> getSubstances un gp
            printfn $"creating: {nm}"
            create gp.GPKODE nm lb (gp.ATCODE.Trim()) an sh rt ss ps
        )


    /// <summary>
    /// Get the GenericProducts for the given Ids
    /// </summary>
    /// <remarks>
    /// This function is memoized
    /// </remarks>
    let get : int list -> GenericProduct [] = Memoization.memoize _get


    /// <summary>
    /// Get the BarCodes for the given GenericProduct
    /// </summary>
    /// <param name="gp">The GenericProduct</param>
    /// <returns>The GPK, BarCodes array</returns>
    let getBarCodes (gp : GenericProduct) =
        gp.PrescriptionProducts
        |> Array.collect (fun pp ->
            pp.TradeProducts
            |> Array.collect (fun tp ->
                tp.ConsumerProducts
                |> Array.collect (fun cp ->
                    cp.BarCodes
                    |> Array.map (fun b -> (gp.Id, b))
                )
            )
        )
        |> Array.groupBy fst
        |> Array.map (fun (gpk, bc) ->
            gpk, bc |> Array.map snd
        )


    let findByBrand brand =
        get []
        |> Array.filter (fun gp ->
            gp.PrescriptionProducts
            |> Array.exists (fun pp ->
                pp.TradeProducts
                |> Array.exists (fun tp->
                    tp.Brand |> String.equalsCapInsens brand
                )
            )
        )