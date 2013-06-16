#r "../../lib/FSharp.Compiler.dll"

#load "SourceFilter.fs"
#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.SourceFilter
open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let t01 = """
#if INTERACTIVE
module Test =
    #load "../FSharpx.TypeProviders/SetupTesting.fsx"
    SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
    #load "__setup__.fsx"
#endif
"""

let t02 = """
#if COMPILED
printfn "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript "aa"

printfn "__setup__.fsx"
#else
#endif
"""
;;

let xs = tokenize t02
let ys = filterComments xs
         |> Seq.iter (fun (KeyValue(pos, s)) -> printfn "l:%O, c:%O, %A" pos.Line pos.Column s);;

printfn "Result:\n%s" <| formatSourceString false t01 config;;

printfn "Result:\n%s" <| formatSelectionFromString false (makeRange 6 5 6 51) t02 config;;

printfn "Tree:\n%A" <| parse false t01;;
