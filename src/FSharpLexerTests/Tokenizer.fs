module Tokenizer

open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type Token = 
   | EOL
   | Token of TokenInformation

let tokenize (content:string) = 
    // Create an interactive checker instance (ignore notifications)
    seq { let sourceTokenizer = SourceTokenizer([ ], "/tmp.fsx")
          let lines = content.Replace("\r\n","\n").Split('\r', '\n')
          let lexState = ref 0L
          for line in lines do 
              let lineTokenizer = sourceTokenizer.CreateLineTokenizer line
              let finLine = ref false
              while not finLine.Value do
                  let tok, newLexState = lineTokenizer.ScanToken(lexState.Value)
                  lexState := newLexState
                  match tok with 
                  | None -> 
                      yield (EOL, System.Environment.NewLine) // new line
                      finLine := true
                  | Some t -> 
                      yield (Token t, line.[t.LeftColumn..t.RightColumn]) }
    |> Seq.toArray