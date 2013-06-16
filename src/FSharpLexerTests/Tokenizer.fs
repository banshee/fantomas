module Tokenizer

open System
open Microsoft.FSharp.Compiler.SourceCodeServices

let tokenize (s : string) =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)

    let fileName = "tmp.fsx"
    let sourceTok = SourceTokenizer([], fileName)

    [| let state = ref 0L
       for n, line in lines |> Seq.zip [1..lines.Length] do
           let tokenizer = sourceTok.CreateLineTokenizer(line)
           let rec parseLine() = seq {
              match tokenizer.ScanToken(!state) with
              | Some(tok), nstate ->
                  let str = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)
                  yield (str, tok, n)
                  state := nstate
                  yield! parseLine()
              | None, nstate -> state := nstate }
           yield! parseLine() |]