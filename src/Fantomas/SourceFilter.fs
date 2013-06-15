module Fantomas.SourceFilter

// This module filters comments and compiler directives based on their locations
// and try to reattach them to the source code while pretty-printing.
//
// After extensive search, the only relevant paper is 
// http://www.cs.kent.ac.uk/projects/refactor-fp/publications/tool-support-for-rfp.pdf
// where comments are preserved after refactoring.
//
// Comments will be preserved as follows:
//   1. Any number of commented lines before a let binding (skipping attributes) will be attached to that binding.
//   2. The same heuristic is done for type declarations and member bindings.
//   3. We would like to attach comments to patterns and expressions, but it's difficult to find out their boundaries 
//      so comments are associated with identifiers.
//   4. Any commented lines in the end of files will be copied to the results.
//
// Tentative solution:
//  1. Lex the source file and obtain a token stream.
//  2. At some keyword tokens such as 'let', 'type', 'member', etc try to go backwards to find comment tokens.
//  3. If we find some attributes, skip them.
//  4. Find first comment token, go backwards until we find a token of another kind (except whitespace tokens).
//  5. If found no comment token, no entry will be created.
//  6. Add blocks of comments into a map keyed by locations of keyword tokens.
// 
// Compiler directives need more thorough treatments.
// Some hindrances are (1) They may or may not have an else branch (2) They can be nested.
// Compiler directives can be looked up by line numbers.
// The problem is to associate line numbers with the AST.

open System
open System.Text
open System.Collections.Generic

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
module Array =
    let inline last xs = 
        if Array.isEmpty xs then
            failwith "Can't get last element on an empty array"
        else
            xs.[xs.Length-1]

type Token = 
    | Token of string * TokenInformation * int
    static member content (Token(s, _, _)) = s
    static member tokenInfo (Token(_, tok, _)) = tok
    static member lineNumber (Token(_, _, n)) = n

/// Return a list of tokens with line and column information
let tokenize (s : string) =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)

    let fileName = "/tmp.fs"
    let sourceTok = SourceTokenizer([], fileName)

    [| let state = ref 0L
       for n, line in lines |> Seq.zip [1..lines.Length] do
           let tokenizer = sourceTok.CreateLineTokenizer(line)
           let rec parseLine() = seq {
              match tokenizer.ScanToken(!state) with
              | Some(tok), nstate ->
                  let str = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)
                  yield Token(str, tok, n)
                  state := nstate
                  yield! parseLine()
              | None, nstate -> state := nstate }
           if not <| String.IsNullOrWhiteSpace line then
               yield parseLine() |> Array.ofSeq |]

/// Search an array starting from the end
let searchBackward f (xs : _ []) =
    let rec loop i =
        if i < 0 then None
        elif f xs.[i] then Some i
        else loop (i - 1)
    loop (xs.Length - 1)

/// Skip all spaces at the end of xs
let (|EndSpaces|) (xs : Token []) =
    let rec loop i =
        if i < 0 then i
        else
            match xs.[i] with
            | Token(_, tok, _) when tok.CharClass = TokenCharKind.WhiteSpace && tok.TokenName = "WHITESPACE" 
                                    || tok.TokenName = "BAR" -> loop (i-1)
            | _ -> i
    xs.[..loop (xs.Length-1)]

/// Recognize an attribute and skip it
let (|Attribute|_|) (EndSpaces xs) =
    if Array.isEmpty xs then None
    else
        match Array.last xs with
        | Token(">]", _, _) -> 
            xs 
            |> searchBackward (fun (Token(s, tok, _)) -> s = "[<" && tok.CharClass = TokenCharKind.Delimiter)
            |> Option.map (fun i -> xs.[..i-1])
        | _ -> None

/// Recognize a list of attributes and return the array fragment before that
let rec (|Attributes|_|) = function
    | Attribute(Attributes xs)
    | Attribute xs -> Some xs
    | _ -> None

/// Return a block of comments and the array fragment before the comment block
let (|CommentBlock|_|) (EndSpaces xs) =
    let rec loop i acc =
        if i < 0 then (acc, i)
        else
            let (Token(_, tok, _)) = xs.[i]
            if tok.CharClass = TokenCharKind.Comment || tok.CharClass = TokenCharKind.LineComment then
                loop (i-1) (xs.[i]::acc)
            else
                (acc, i)
    match loop (xs.Length-1) [] with
    | [], _ -> None
    | ts, i -> Some(ts, xs.[..i])

/// Merge a list of token lists based on their line numbers 
let mergeTokens tss =
    tss
    |> Seq.concat
    |> Seq.groupBy Token.lineNumber 
    |> Seq.map (snd >> Seq.map Token.content >> String.concat "")
    |> Seq.map (fun s -> s.TrimEnd('\r'))

/// Retrieve comments in a list of lines and skip some whitespaces
let rec (|CommentBlocks|_|) xs =
    let rec (|CommentBlocks|_|) = function
        | CommentBlock(ts, CommentBlocks(tss, xs)) -> Some(ts::tss, xs)
        | CommentBlock(ts, xs) -> Some([ts], xs)
        | _ -> None
    match xs with
    | CommentBlocks(tss, xs) -> 
        let ts = tss |> List.rev |> mergeTokens
        Some(ts, xs)
    | _ -> None

/// Member declarations of all kinds
let (|MemberToken|_|) (Token(s, tok, n)) =
    match s with
    | "member" | "abstract" | "default" | "override"
    | "static" | "interface" | "new" | "val" 
    | "inherit" when tok.CharClass = TokenCharKind.Keyword -> Some(s, tok, n)
    | _ -> None

/// Pick the closest identifier
let tryPickIdentifier xs =
    Array.tryPick (fun (Token(s, tok, n)) -> 
        if tok.CharClass = TokenCharKind.Identifier then Some(s, tok, n) else None) xs

/// Keyword and identifier tokens have their own attached comments
let (|SupportedToken|_|) (Token(s, tok, n)) =
    if tok.CharClass = TokenCharKind.Keyword || tok.CharClass = TokenCharKind.Identifier then Some(s, tok, n)
    else None

/// Given a list of tokens, attach comments to appropriate positions
let filterComments xss =
    let rec loop i (xs : Token []) (dic : Dictionary<_, _>)  = 
        if i <= 0 then dic
        else
            match xs.[i] with
            // Attach comments to members
            | MemberToken(_, tok1, n1) ->
                match xs.[..i-1] with
                | Attributes(CommentBlocks(ts, xs'))
                | CommentBlocks(ts, xs') ->
                    dic.Add(mkPos n1 tok1.LeftColumn, ts)
                    match tryPickIdentifier xs.[i..] with
                    | Some(_, tok2, n2) ->
                        // This is a hack to ensure that one of the keys will be captured.
                        // Right now members on fs and fsi files have different ranges.
                        dic.Add(mkPos n2 tok2.LeftColumn, ts)
                        loop (xs'.Length-1) xs' dic
                    | _ -> loop (xs'.Length-1) xs' dic
                | _ -> loop (i-1) xs dic          
            | SupportedToken(_, tok, n) ->
                match xs.[..i-1] with
                | Attributes(CommentBlocks(ts, xs'))
                | CommentBlocks(ts, xs') ->
                    dic.Add(mkPos n tok.LeftColumn, ts)
                    loop (xs'.Length-1) xs' dic
                | _ -> loop (i-1) xs dic           
            | _ -> loop (i-1) xs dic
    let xs = Array.concat xss
    loop (xs.Length-1) xs (Dictionary())

/// Neccessary information for reconstructing compiler directives
type Directive =
    // #if <string> ... #endif
    | If of string
    // #if <string> ... #else <string seq> #endif
    | IfElse of string * string seq

/// Skip all spaces at the beginning of xs
let (|BeginSpaces|) (xs : Token []) =
    let rec loop i =
        if i >= xs.Length then i
        else
            match xs.[i] with
            | Token(_, tok, _) when tok.CharClass = TokenCharKind.WhiteSpace && tok.TokenName = "WHITESPACE" 
                                    || tok.TokenName = "BAR" -> loop (i+1)
            | _ -> i
    xs.[loop 0..]

/// Recognize #if in a token line
let (|IfBranch|_|) (xss : Token [] []) = 
    if Array.isEmpty xss then None
    else
        let (BeginSpaces xs) = xss.[0]
        if Array.isEmpty xs then None
        else
            match xs.[0] with
            | Token("#if", tok, _) when tok.TokenName = "HASH_IF" ->
                match tryPickIdentifier xs with
                | Some(s, _, n) -> Some(n+1, s, xss.[1..])
                | _ -> None
            | _ -> None

/// A code block consists of a few lines between two hash directives
let (|CodeBlock|_|) (xss : Token [] []) =
    let rec loop i =
        if i >= xss.Length then i
        else
            let (BeginSpaces xs) = xss.[i]
            if Array.isEmpty xs then loop (i+1)
            else
                match xs.[0] with
                | Token(_, tok, _) when tok.TokenName = "HASH_IF" -> i
                | _ -> loop (i+1)
    match loop 0 with
    | 0 -> None
    | i -> Some(xss.[..i-1], xss.[i..])

/// Recognize the #else directive
let (|ElseBranch|_|) (xss : Token [] []) = 
    if Array.isEmpty xss then None
    else
        let (BeginSpaces xs) = xss.[0]
        if Array.isEmpty xs then None
        else
            match xs.[0] with
            | Token("#else", tok, n) when tok.TokenName = "HASH_IF" ->
                Some(n-1, xss.[1..])
            | _ -> None

/// Recognize the #endif directive
let (|EndIfBranch|_|) (xss : Token [] []) = 
    if Array.isEmpty xss then None
    else
        let (BeginSpaces xs) = xss.[0]
        if Array.isEmpty xs then None
        else
            match xs.[0] with
            | Token("#endif", tok, n) when tok.TokenName = "HASH_IF" -> Some(n-1, xss.[1..])
            | _ -> None

/// Build up directives from a list of token lines
let (|Directive|_|) = function
    | IfBranch(start, s, CodeBlock(_, EndIfBranch(finish, xss))) -> 
        Some(((start, finish), If s), xss)
    | IfBranch(start, s, CodeBlock(_, ElseBranch(finish, CodeBlock(tss, EndIfBranch(_, xss))))) -> 
        Some(((start, finish), IfElse(s, mergeTokens tss)), xss)
    | _ -> None

/// Get all directives and attach them to appropriate line numbers
let filterDirectives xss =
    let rec loop (xss : Token [] []) (dic : Dictionary<_, _>)  = 
        match xss with
        | Directive((r, d), xss)
        | CodeBlock(_, Directive((r, d), xss)) -> 
            dic.Add(r, d)
            loop xss dic
        | _ -> dic
    loop xss (Dictionary())

let collectCommentsAndDirectives s =
    let tokens = tokenize s
    (filterComments tokens, filterDirectives tokens)

let filterDefines xss =
    let rec loop (xss : Token [] []) (hs : HashSet<_>)  = 
        match xss with
        | IfBranch(_, s, xss) ->
            hs.Add(sprintf "--define:%s" s) |> ignore
            loop xss hs
        | ElseBranch(_, xss)
        | EndIfBranch(_, xss)
        | CodeBlock(_, xss) -> 
            loop xss hs
        | _ -> hs
    let hs = loop xss (HashSet())
    Seq.toArray hs

let collectDefines s =
    filterDefines (tokenize s)
    
