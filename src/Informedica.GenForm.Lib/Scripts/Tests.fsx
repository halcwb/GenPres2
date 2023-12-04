
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"

#r "../../Informedica.ZIndex.Lib/bin/Debug/net6.0/Informedica.ZIndex.Lib.dll"
#r "../../Informedica.ZForm.Lib/bin/Debug/net6.0/Informedica.ZForm.Lib.dll"
#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"
#r "../../Informedica.GenCore.Lib/bin/Debug/net6.0/Informedica.GenCore.Lib.dll"
#r "../../Informedica.GenForm.Lib/bin/Debug/net6.0/Informedica.GenForm.Lib.dll"


//#load "load.fsx"

#time


open System
open Informedica.Utils.Lib


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__



module Expecto =

    open Expecto

    let run = runTestsWithCLIArgs [] [| "--summary" |]



module Tests =


    open MathNet.Numerics
    open Expecto
    open Expecto.Flip

    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib
    open Informedica.GenForm.Lib


    module PatientCategoryTests =

        open System.Security.Cryptography.X509Certificates

        let tests = testList "PatientCategory" [
            let filter = Filter.filter
            let patCat =
                {
                    Department = None
                    Diagnoses = [||]
                    Gender = AnyGender
                    Age = MinMax.none
                    Weight = MinMax.none
                    BSA = MinMax.none
                    GestAge = MinMax.none
                    PMAge = MinMax.none
                    Location = AnyAccess
                }

            test "an empty filter and empty patient category" {
                patCat
                |> PatientCategory.filter filter
                |> Expect.isTrue "should return true"
            }

            test "an empty filter and patient category with female gender" {
                { patCat with Gender = Female }
                |> PatientCategory.filter filter
                |> Expect.isFalse "should return false"
            }

            test "a filter with female gender and patient category with female gender" {
                { patCat with Gender = Female }
                |> PatientCategory.filter { filter with Gender = Female }
                |> Expect.isTrue "should return true"
            }

            test "a filter with female gender and patient category with no gender" {
                { patCat with Gender = AnyGender }
                |> PatientCategory.filter { filter with Gender = Female }
                |> Expect.isTrue "should return true"
            }

            test "an empty filter and a patient category with a max age of 7" {
                { patCat with Age = { patCat.Age with Maximum  = Some 7N  } }
                |> PatientCategory.filter filter
                |> Expect.isFalse "should return false"
            }

            test "a filter with age 5 and a patient category with a max age of 7" {
                { patCat with Age = { patCat.Age with Maximum  = Some 7N  } }
                |> PatientCategory.filter { filter with AgeInDays = Some 5N }
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 5 and a patient category with a min age of 7" {
                { patCat with Age = { patCat.Age with Minimum  = Some 7N  } }
                |> PatientCategory.filter { filter with AgeInDays = Some 5N }
                |> Expect.isFalse "should return false"
            }

            test "a filter with age 5 and a patient category with a min age of 3 and max age of 7" {
                { patCat with Age = { patCat.Age with Minimum  = Some 3N; Maximum = Some 7N } }
                |> PatientCategory.filter { filter with AgeInDays = Some 5N }
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 5 with a patient category with a min age of 3 and max age of 7 and gender female" {
                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 3N
                            Maximum = Some 7N
                        }
                    Gender = Female
                }
                |> PatientCategory.filter { filter with AgeInDays = Some 5N }
                |> Expect.isFalse "should return false"
            }

            test "a filter with age 5, gender female with a patient category with a min age of 3 and max age of 7 and gender female" {
                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 3N
                            Maximum = Some 7N
                        }
                    Gender = Female
                }
                |> PatientCategory.filter { filter with AgeInDays = Some 5N; Gender = Female }
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 0 and gestational age 30 weeks with a patient category with a min age of 0 and max age of 28 and gestational age min 28 and max 32 weeks" {
                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 0N
                            Maximum = Some 28N
                        }
                    GestAge =
                        { patCat.GestAge with
                            Minimum  = Some (28N * 7N)
                            Maximum = Some (32N * 7N)
                        }
                }
                |> PatientCategory.filter { filter with AgeInDays = Some 0N; GestAgeInDays = Some (30N * 7N) }
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 0 and gestational age 37 weeks with a patient category with a min age of 0 and max age of 28 and gestational age min 28 and max 32 weeks" {
                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 0N
                            Maximum = Some 28N
                        }
                    GestAge =
                        { patCat.GestAge with
                            Minimum  = Some (28N * 7N)
                            Maximum = Some (32N * 7N)
                        }
                }
                |> PatientCategory.filter { filter with AgeInDays = Some 0N; GestAgeInDays = Some (37N * 7N) }
                |> Expect.isFalse "should return false"
            }

            test "a filter with age 8 and gestational age 27 weeks with a patient category with a min age of 0 and max age of 28 and gestational age min 28 and max 32 weeks" {
                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 0N
                            Maximum = Some 28N
                        }
                    GestAge =
                        { patCat.GestAge with
                            Minimum  = Some (28N * 7N)
                            Maximum = Some (32N * 7N)
                        }
                }
                |> PatientCategory.filter { filter with AgeInDays = Some 8N; GestAgeInDays = Some (27N * 7N) }
                |> Expect.isFalse "should return false"
            }

            test "a filter with age 0 and gestational age 30 weeks with a patient category with a min age of 0 and max age of 28 and pm age min 28 and max 32 weeks" {
                let filter =
                    { filter with AgeInDays = Some 0N; GestAgeInDays = Some (30N * 7N) }
                    |> Filter.calcPMAge

                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 0N
                            Maximum = Some 28N
                        }
                    PMAge =
                        { patCat.PMAge with
                            Minimum  = Some (28N * 7N)
                            Maximum = Some (32N * 7N)
                        }
                }
                |> PatientCategory.filter filter
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 0 and gestational age 37 weeks with a patient category with a min age of 0 and max age of 28 and pm age min 28 and max 32 weeks" {
                let filter =
                    { filter with AgeInDays = Some 0N; GestAgeInDays = Some (37N * 7N) }
                    |> Filter.calcPMAge

                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 0N
                            Maximum = Some 28N
                        }
                    PMAge =
                        { patCat.PMAge with
                            Minimum  = Some (28N * 7N)
                            Maximum = Some (32N * 7N)
                        }
                }
                |> PatientCategory.filter filter
                |> Expect.isFalse "should return false"
            }

            test "a filter with age 8 and gestational age 27 weeks with a patient category with a min age of 0 and max age of 28 and pm age min 28 and max 32 weeks" {
                let filter =
                    { filter with AgeInDays = Some 8N; GestAgeInDays = Some (27N * 7N) }
                    |> Filter.calcPMAge

                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum  = Some 0N
                            Maximum = Some 28N
                        }
                    PMAge =
                        { patCat.PMAge with
                            Minimum  = Some (28N * 7N)
                            Maximum = Some (32N * 7N)
                        }
                }
                |> PatientCategory.filter filter
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 0, ga = 32 and weight 1.45 with a patient category with max age = 30 and max gest 37 and max weight 1.5" {
                let filter =
                    { filter with
                        AgeInDays = Some 0N
                        GestAgeInDays = Some (32N * 7N)
                        WeightInGram = Some 1450N
                    }

                { patCat with
                    Age =
                        { patCat.Age with
                            Maximum = Some 30N
                        }
                    GestAge =
                        { patCat.GestAge with
                            Maximum = Some (37N * 7N)
                        }
                    Weight =
                        { patCat.Weight with
                            Maximum = Some (1500N)
                        }
                }
                |> PatientCategory.filter filter
                |> Expect.isTrue "should return true"
            }

            test "a filter with age 0, ga = 32 and weight 1.45 with a patient category with min age = 30 and max age = 720" {
                let filter =
                    { filter with
                        AgeInDays = Some 0N
                        GestAgeInDays = Some (32N * 7N)
                        WeightInGram = Some 1450N
                    }

                { patCat with
                    Age =
                        { patCat.Age with
                            Minimum = Some 30N
                            Maximum = Some 720N
                        }
                }
                |> PatientCategory.filter filter
                |> Expect.isFalse "should return false"
            }
        ]


    [<Tests>]
    let tests = testList "GenForm Tests" [
        PatientCategoryTests.tests
    ]



Tests.tests
|> Expecto.run