
#load "load.fsx"

#r "../bin/Debug/net9.0/Informedica.GenForm.Lib.dll"


open System
open Informedica.GenForm.Lib


Environment.SetEnvironmentVariable("GENPRES_URL_ID", "1s76xvQJXhfTpV15FuvTZfB-6pkkNTpSB30p51aAca8I")



Api.cachedApiProvider.GetResourceInfo ()

Api.reloadCache ()

Api.getProducts ()
|> Array.filter (fun p -> p.Generic = "noradrenaline")