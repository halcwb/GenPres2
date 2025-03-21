
#load "load.fsx"

open System

let dataUrlId = "1IZ3sbmrM4W4OuSYELRmCkdxpN9SlBI-5TLSvXWhHVmA"
Environment.SetEnvironmentVariable("GENPRES_PROD", "1")
Environment.SetEnvironmentVariable("GENPRES_URL_ID", dataUrlId)



#load "../Types.fs"
#load "../Utils.fs"
#load "../Mapping.fs"
#load "../Mapping.fs"
#load "../Patient.fs"
#load "../LimitTarget.fs"
#load "../DoseType.fs"
#load "../Product.fs"
#load "../Filter.fs"
#load "../DoseRule.fs"
#load "../SolutionRule.fs"
#load "../RenalRule.fs"

#time


open Informedica.GenForm.Lib


open Informedica.Utils.Lib





RenalRule.get ()

Web.getDataFromSheet dataUrlId "RenalRules"

Web.GoogleSheets.createUrl "RenalRules" (Web.getDataUrlIdGenPres ())