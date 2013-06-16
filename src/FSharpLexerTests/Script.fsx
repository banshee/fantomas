#r "../../lib/FSharp.Compiler.dll"

#load "Tokenizer.fs"

open Tokenizer

let t01 = """
#if INTERACTIVE
printfn "Hello"
#else
printfn "World"
#endif 
"""

let xs = tokenize t01;;
printfn "Number of tokens = %i:\n %A" xs.Length xs;;
