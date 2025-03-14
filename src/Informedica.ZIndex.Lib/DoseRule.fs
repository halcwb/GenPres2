namespace Informedica.ZIndex.Lib


module DoseRule =

    open System

    open Aether

    open Informedica.Utils.Lib.BCL
    open Informedica.Utils.Lib
    open ConsoleWriter.NewLineNoTime


    module Constants =

        [<Literal>]
        let intensive = "intensieve"

        [<Literal>]
        let nonIntensive = "niet-intensieve"

        [<Literal>]
        let all = "alle"

        [<Literal>]
        let profylactic = "profylactisch"

        [<Literal>]
        let therapeutic = "therapeutisch"

        [<Literal>]
        let standard = "Standaard"

        [<Literal>]
        let special = "Verbyzondering"

        [<Literal>]
        let male = "man"

        [<Literal>]
        let female = "vrouw"


    /// <summary>
    /// Get the most extreme min and max values from an array of MinMax.
    /// </summary>
    /// <param name="xs">The array of MinMax</param>
    /// <returns>The most extreme MinMax as a RuleMinMax</returns>
    let foldMinMax xs =
        xs |> Array.fold (fun { Min = min; Max = max} (acc: RuleMinMax) ->
            { Min = Option.min acc.Min min; Max = Option.max acc.Max max }
        ) { Min = None; Max = None }


    /// Get the string representation of a RuleFrequency.
    let freqToString (freq : RuleFrequency) =
        (string freq.Frequency) + " " + freq.Time


    /// <summary>
    /// Get the string representation of a DoseRule.
    /// </summary>
    /// <param name="del">A delimiter</param>
    /// <param name="dr">The DoseRule</param>
    let toString del (dr: DoseRule)  =
        let minMaxToString n u p (mm: RuleMinMax) s =
            let mms =
                match mm.Min, mm.Max with
                | Some min, Some max ->
                    let min = Double.fixPrecision p min |> string
                    let max = Double.fixPrecision p max |> string
                    $"%s{min} - %s{max}"
                | Some min, None ->
                    let min = Double.fixPrecision p min |> string
                    sprintf "vanaf %s" min
                | None, Some max ->
                    if max = 0. then ""
                    else
                        let max = Double.fixPrecision p max |> string
                        $"tot %s{max}"
                | None, None -> ""
            if mms = "" then s
            else
                s + n + ": " + mms + " " + u + del

        let adds s1 s2 s3 =
            if s2 |> String.IsNullOrWhiteSpace then s3
            else
                let s3 = if s1 = "" then s3 else s3 + s1 + ": "
                s3 + s2 + del

        let gp =
            dr.GenericProduct |> Seq.fold(fun a gp ->
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let pp =
            dr.PrescriptionProduct |> Seq.fold(fun a gp ->
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let tp =
            dr.TradeProduct |> Seq.fold(fun a gp ->
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let s = "" + (string dr.Id) + del
        let s = s |> adds "" gp
        let s = s |> adds "" pp
        let s = s |> adds "" tp
        let s = s |> adds "Gebruik" dr.Usage
        let s = s |> adds "Groep" dr.CareGroup
        let s = s |> adds "Type" dr.DoseType
        let s = s |> adds "Route" (dr.Routes |> String.concat "/")
        let s = s |> adds "Indicatie" dr.Indication

        let s = if dr.HighRisk then s + "Hig Risk " else s

        let s = s |> adds "" dr.Gender

        let s = s |> minMaxToString "Leeftijd" "maanden" 1 dr.Age
        let s = s |> minMaxToString "Gewicht" "kg" 3 dr.Weight
        let s = s |> minMaxToString "BSA" "m2" 3 dr.BSA

        let s =
            if dr.Freq.Frequency <= 0. then s
            else
                s + "Freq: " + (dr.Freq |> freqToString) + " " + del

        let s = s |> minMaxToString "Norm" dr.Unit 3 dr.Norm
        let s = s |> minMaxToString "Norm/Kg" dr.Unit 3 dr.NormKg
        let s = s |> minMaxToString "Abs" dr.Unit 3 dr.Abs
        let s = s |> minMaxToString "Abs/Kg" dr.Unit 3 dr.AbsKg

        let s = s |> minMaxToString "Norm/m2" dr.Unit 3 dr.NormM2
        let s = s |> minMaxToString "Abs/m2" dr.Unit 3 dr.AbsM2

        let s = s |> String.subString 0 ((s |> String.length) - (del |> String.length))
        s


    /// An empty RuleMinMax.
    let minmax = { Min = None; Max = None }


    /// Create a RuleProduct.
    let createProduct id nm : RuleProduct = { Id = id; Name = nm }


    /// Create a RuleGenericProduct.
    let createGenericProduct id nm rt un sl = { Id = id; Name = nm; Route = rt; Unit = un; Substances = sl }


    /// Create a RuleFrequency.
    let createFrequency fr tm = { Frequency = fr; Time = tm }


    /// Create a RuleMinMax.
    let createMinMax min max =
        let check =
            max
            |> string
            |> String.forall (fun c -> c = '9' || c = '.')

        if max < min then minmax
        else
            let mn = if min = 0. then None else Some min
            let mx = if max = 0. || check then None else Some max

            { Min = mn; Max = mx }


    /// Create a DoseRule.
    let create id gr us dt gp pr tr rt ci ic hr sx ag wt bs fr no ab nk ak nm am un =
        {
            Id = id
            CareGroup = gr
            Usage = us
            DoseType = dt
            GenericProduct = gp
            PrescriptionProduct = pr
            TradeProduct = tr
            Routes = rt
            IndicationId = ci
            Indication = ic
            HighRisk = hr
            Gender = sx
            Age = ag
            Weight = wt
            BSA = bs
            Freq = fr
            Norm = no
            Abs = ab
            NormKg = nk
            AbsKg = ak
            NormM2 = nm
            AbsM2 = am
            Unit = un
        }


    /// An empty DoseRule.
    let empty =
        {
            Id = 0
            CareGroup = ""
            Usage = ""
            DoseType = ""
            GenericProduct = Array.empty
            PrescriptionProduct = Array.empty
            TradeProduct = Array.empty
            Routes = [||]
            IndicationId = 0
            Indication = ""
            HighRisk = false
            Gender = ""
            Age = minmax
            Weight = minmax
            BSA = minmax
            Freq = createFrequency 0. ""
            Norm = minmax
            Abs = minmax
            NormKg = minmax
            AbsKg = minmax
            NormM2 = minmax
            AbsM2 = minmax
            Unit = ""
        }


    let _getGenericProducts () =
        GenPresProduct.getGenericProducts ()
        |> Array.map (fun gp ->
            let unt =
                gp.Substances
                |> Array.fold (fun acc s ->
                    if acc = "" then s.ShapeUnit else acc
                ) ""
            {
                Id = gp.Id
                Name = gp.Name
                Route = gp.Route
                Unit = unt
                Substances =
                    gp.Substances
                    |> Array.map (fun s ->
                        {
                            Name = s.SubstanceName
                            Quantity = s.SubstanceQuantity
                            Unit = s.SubstanceUnit
                        }
                    )
            }
        )


    /// <summary>
    /// Map all GenericProducts from all GenPresProducts to RuleGenericProducts.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let getGenericProducts : unit -> RuleGenericProduct[] =
        Memoization.memoize _getGenericProducts


    let _getPrescriptionProducts _ =
        query {
            for p in Zindex.BST052T.records () do
            join nm in Zindex.BST020T.records ()
                on (p.PRNMNR = nm.NMNR)
            where (p.MUTKOD <> 1)
            select
                (
                    createProduct
                        p.PRKODE
                        (nm.NMNAAM.Trim())
                )
        } |> Seq.toArray


    /// <summary>
    /// Map all PrescriptionProducts RuleProducts.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let getPresciptionProducts : unit -> RuleProduct[] =
        Memoization.memoize _getPrescriptionProducts


    let _getTradeProducts _ =
        query {
            for p in Zindex.BST031T.records () do
            join nm in Zindex.BST020T.records ()
                on (p.HPNAMN = nm.NMNR)
            where (p.MUTKOD <> 1)
            select
                (
                    createProduct
                        p.HPKODE
                        (nm.NMNAAM.Trim())
                )
        } |> Seq.toArray


    /// <summary>
    /// Map all TradeProducts to RuleProducts.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let getTradeProducts : unit -> RuleProduct[] =
        Memoization.memoize _getTradeProducts


    /// Get all specifid DoseRule routes for a BST642T record.
    let getICPCRoute (icp : Zindex.BST642T.BST642T) =
        let r = Names.getThes icp.GPKTWG Names.Route Names.Fifty
        if r = "TOEDIENINGSWEG NIET INGEVULD" ||
           r = "PARENTERAAL" then Array.empty
        else [|r|]
        |> Array.collect (fun r ->
            r
            |> String.splitAt ','
            |> Array.map String.trim
        )
        |> Array.distinct


    /// Get the DoseType for a BST641T record.
    let getDoseType (bas : Zindex.BST641T.BST641T) =
        Zindex.BST902T.records ()
        |> Array.tryFind (fun tx ->
            tx.MUTKOD <> 1 &&
            tx.TSNR = bas.GPDCTH &&
            tx.TSITNR = bas.GPDCOD
        )
        |> (fun r ->
            if r |> Option.isNone then ""
            else r.Value.THNM50.Trim())


    /// Get the dose text for a BST642T record.
    let getICPCText (icp : Zindex.BST642T.BST642T) =
        Zindex.BST380T.records ()
        |> Array.tryFind (fun i ->
            i.ICPCNR1 = icp.ICPCNR1
        )
        |> (fun r ->
            if r.IsNone then ""
            else r.Value.ICPCTXT.Trim()
        )


    /// Get the RuleFrequency for a BST643T record.
    let getFrequency (cat: Zindex.BST643T.BST643T) =
        Zindex.BST360T.records ()
        |> Array.tryFind (fun tx ->
            tx.MUTKOD <> 1 &&
            tx.TTEHNR = cat.GPDFEE
        )
        |> (fun r ->
            if r |> Option.isNone then ""
            else r.Value.TTEHOM.Trim())

        |> String.replace "om de dag" "per 2 dagen"
        |> String.replace "per half uur" "per 30 minuten"
        |> createFrequency cat.GPDFAA


    let parse gpks =

        query {
            // get all dose records
            for dos in  Zindex.BST649T.records () do
            // get 1 to 1 all category records
            join cat in Zindex.BST643T.records ()
                on (dos.GPDDNR = cat.GPDDNR)
            // get many to 1 all dose indications
            join icp in Zindex.BST642T.records ()
                on (cat.GPDCAT = icp.GPDCAT)
            // get many to 1 all prescription and trade products
            join bas in Zindex.BST641T.records ()
                on (icp.GPDBAS = bas.GPDBAS)
            // get many to 1 all generic products
            join vas in Zindex.BST640T.records ()
                on (bas.GPKODE = vas.GPKODE)

            where (dos.MUTKOD <> 1 &&
                   cat.MUTKOD <> 1 &&
                   icp.MUTKOD <> 1 &&
                   bas.MUTKOD <> 1 &&
                   vas.MUTKOD <> 1 &&
                   gpks |> Array.exists ((=) bas.GPKODE))

            select
                ((bas.GPKODE, bas.PRKODE, bas.HPKODE),
                {
                    empty with
                        Id           = dos.GPDDNR
                        Routes       = getICPCRoute icp
                        DoseType     = getDoseType bas
                        IndicationId = icp.GPDID1
                        Indication   = getICPCText icp
                        HighRisk     = vas.GPRISC = "*"

                        Gender =
                            if   vas.GPDGST = 1 then Constants.male
                            elif vas.GPDGST = 2 then Constants.female
                            else ""

                        CareGroup =
                            if   icp.GPDZCO = 1 then Constants.nonIntensive
                            elif icp.GPDZCO = 2 then Constants.intensive
                            else Constants.all

                        Usage =
                            if   icp.ICPCTO = 0 then ""
                            elif icp.ICPCTO = 1 then Constants.profylactic
                            else Constants.therapeutic

                        Age    = createMinMax cat.GPDLFM cat.GPDLFX
                        Weight = createMinMax cat.GPDKGM cat.GPDKGX
                        BSA    = createMinMax cat.GPDM2M cat.GPDM2X
                        Freq   = getFrequency cat
                        Norm   = createMinMax dos.GPNRMMIN dos.GPNRMMAX
                        Abs    = createMinMax dos.GPABSMIN dos.GPABSMAX
                        NormKg = createMinMax dos.GPNRMMINK dos.GPNRMMAXK
                        AbsKg  = createMinMax dos.GPABSMINK dos.GPABSMAXK
                        NormM2 = createMinMax dos.GPNRMMINM dos.GPNRMMAXM
                        AbsM2  = createMinMax dos.GPABSMINM dos.GPABSMAXM
                })
        }
        |> Seq.toArray
        // Get Generic products
        |> Array.map ((fun (bas, r) ->
            let gpk, _, _ = bas
            let gpks =
                getGenericProducts ()
                |> Array.filter (fun gp ->
                        gp.Id = gpk
                    )
            let rt =
                if (r.Routes |> Array.isEmpty)
                then
                    gpks
                    |> Array.collect _.Route
                else r.Routes
            let un =
                match gpks |> Array.tryHead with
                | Some gp -> gp.Unit
                | None -> ""
            let r =
                {
                    r with
                        GenericProduct = gpks
                        Routes = rt
                        Unit = un
                }
            (bas, r)
        // Get prescription products
         ) >> (fun (bas, r) ->
            let _, prk, _ = bas
            let prks =
                getPresciptionProducts ()
                |> Array.filter (fun pp ->
                    pp.Id = prk
                )
            (bas, { r with PrescriptionProduct = prks })
        // Get trade products
        ) >> (fun (bas, r) ->
            let _, _, hpk = bas
            let hpks =
                getTradeProducts ()
                |> Array.filter (fun tp ->
                    tp.Id = hpk
                )
            { r with TradeProduct = hpks }
        ))


    let _get gpks =
        let useDemo = FilePath.useDemo()

        fun () ->
            if FilePath.ruleCache useDemo |> File.exists then
                FilePath.ruleCache useDemo
                |> Json.getCache<DoseRule[]>
            else
                let p = FilePath.ruleCache useDemo
                writeInfoMessage $"No {p}, creating DoseRule"
                let rules = GenPresProduct.getGPKS gpks |> parse
                writeInfoMessage $"Created {rules |> Array.length} Rules"

                rules |> Json.cache p
                rules
        |> StopWatch.clockFunc "Getting DoseRules"


    /// <summary>
    /// Get all DoseRules for a list of GPKS.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let get : List<int> -> DoseRule [] = Memoization.memoize _get


    /// Load all DoseRules in memory.
    let load = get >> ignore


    /// Get the string representation of a DoseRule.
    let toString2 (dr : DoseRule) =
        let addString lbl s =
            if s = "" then ""
            else
                lbl + ": " + s + ", "

        let freqToString (fr: RuleFrequency) =
            (fr.Frequency |> string) + " " + (fr.Time |> string)

        let minMaxToString u (mm: RuleMinMax) =
            let s =
                match mm.Min, mm.Max with
                | None, None -> ""
                | Some min, None -> "vanaf " + (min |> string)
                | None, Some max ->
                    if max = 0. then "" else "tot " + (max |> string)
                | Some min, Some max -> (min |> string) + " - " + (max |> string)
            if s = "" then "" else s + " " + u

        if dr.GenericProduct |> Array.length = 1 then
            dr.GenericProduct[0].Name + ": "
        else ""
        + (addString "Indicatie" dr.Indication)
        + (addString "Geslacht" dr.Gender)
        + (addString "Leeftijd" (dr.Age |> minMaxToString "maanden"))
        + (addString "Oppervlak" (dr.BSA |> minMaxToString "m2"))
        + (addString "Gewicht" (dr.Weight |> minMaxToString "kg"))
        + (addString "Frequentie" (dr.Freq |> freqToString))
        + (addString "Dosering" (dr.Norm |> minMaxToString dr.Unit))
        + (addString "Dose Per kg" (dr.NormKg |> minMaxToString dr.Unit))
        + (addString "Dose Per m2" (dr.NormM2 |> minMaxToString dr.Unit))
        + (addString "Grens Per kg" (dr.AbsKg |> minMaxToString dr.Unit))
        + (addString "Grens Per m2" (dr.AbsM2 |> minMaxToString dr.Unit))
        + (addString "Abs grens" (dr.Abs |> minMaxToString dr.Unit))
        |> String.remove 1


    let indications_ () =
        // Get all distinct indications
        Zindex.BST642T.records ()
        |> Array.map getICPCText
        |> Array.distinct
        |> Array.sort


    /// <summary>
    /// Get all DoseRule indications.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let indications = Memoization.memoize indications_


    let routes_ () =
        Zindex.BST642T.records ()
        |> Array.collect getICPCRoute
        |> Array.distinct
        |> Array.sort




    /// <summary>
    /// Get all DoseRule routes.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let routes = Memoization.memoize routes_


    let frequencies_ () =
        Zindex.BST643T.records ()
        |> Array.map getFrequency
        |> Array.distinct
        |> Array.sortBy (fun f -> (f.Time, f.Frequency))



    /// <summary>
    /// Get all DoseRule frequencies.
    /// </summary>
    /// <remarks>
    /// This is a memoized function.
    /// </remarks>
    let frequencies = Memoization.memoize frequencies_



    module Optics =


        let getWeight = Optic.get DoseRule.Weight_


        let setWeight = Optic.set DoseRule.Weight_


        let getBSA = Optic.get DoseRule.BSA_


        let setBSA = Optic.set DoseRule.BSA_


        let getNorm = Optic.get DoseRule.Norm_


        let setNorm = Optic.set DoseRule.Norm_


        let getAbs = Optic.get DoseRule.Abs_


        let setAbs = Optic.set DoseRule.Abs_


        let getNormKg = Optic.get DoseRule.NormKg_


        let setNormKg = Optic.set DoseRule.NormKg_


        let getAbsKg = Optic.get DoseRule.AbsKg_


        let setAbsKg = Optic.set DoseRule.AbsKg_


        let getNormM2 = Optic.get DoseRule.NormM2_


        let setNormM2 = Optic.set DoseRule.NormM2_


        let getAbsM2 = Optic.get DoseRule.AbsM2_


        let setAbsM2 = Optic.set DoseRule.AbsM2_