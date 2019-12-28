module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type HangmanState = Domain.HangmanState
type SearchPhrase = Domain.SearchPhrase
type EnteredLetters = Domain.EnteredLetters

let read (input : string) =
    match input with
    | Increment -> Domain.Increment |> DomainMessage
    | Decrement -> Domain.Decrement |> DomainMessage
    | IncrementBy v -> Domain.IncrementBy v |> DomainMessage
    | DecrementBy v -> Domain.DecrementBy v |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update : Domain.Message -> State -> State) (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        let message = sprintf "The message was %A. New state is %A" msg newState
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let printHangman (state: HangmanState) = 
    printf ""
    state

let rec print (state: HangmanState, phrase: SearchPhrase, enteredLetters: EnteredLetters) =
    match phrase with
    | Domain.Cons(letter, tail) ->
        if Domain.ListContains(enteredLetters, letter) then
            printf "%c" letter
        else
            printf "_"
        print(state, tail, enteredLetters)
    | _ ->
        printfn "\n\n"
        printHangman(state)

let rec loop (state : State) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update state
    |> print
    |> loop
