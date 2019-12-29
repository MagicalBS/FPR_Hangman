module Printer

open System
open Domain

let printHangman' (state: HangmanState) = 
    match state with 
    | Initial ->
        System.IO.File.ReadAllText "HangmanAscii/Initial.txt"
        |> Console.WriteLine
    | Gallows ->
        System.IO.File.ReadAllText "HangmanAscii/Gallows.txt"
        |> Console.WriteLine
    | Rope ->
        System.IO.File.ReadAllText "HangmanAscii/Rope.txt"
        |> Console.WriteLine
    | Head ->
        System.IO.File.ReadAllText "HangmanAscii/Head.txt"
        |> Console.WriteLine
    | Arms ->
        System.IO.File.ReadAllText "HangmanAscii/Arms.txt"
        |> Console.WriteLine
    | Body ->
        System.IO.File.ReadAllText "HangmanAscii/Body.txt"
        |> Console.WriteLine
    | Legs ->
        System.IO.File.ReadAllText "HangmanAscii/Legs.txt"
        |> Console.WriteLine
    | Dead ->
        System.IO.File.ReadAllText "HangmanAscii/Dead.txt"
        |> Console.WriteLine
    | Success -> 
        failwith "Invalid State"
    state

let printHangman (result: InputState) = 
    match result with 
    | Unverified _ -> 
        failwith "Invalid State"
    | Correct (state, searchPhrase, enteredLetters, character) -> 
        printHangman' state |> ignore
        Correct (state, searchPhrase, enteredLetters, character)
    | Incorrect (state, searchPhrase, enteredLetters, character) -> 
        printHangman' state |> ignore
        Incorrect (state, searchPhrase, enteredLetters, character)
    | Invalid err ->
        Invalid err

let printWonOrLost (state: GameState) : GameState = 
    match state with
    | Won phrase -> 
        printfn "Congratulations, you won."
        initGame
    | Lost phrase -> 
        printfn "Congratulations, you dead." 
        initGame
    | _ -> 
        state
        
let rec printPhrase' (state: HangmanState, phrase: SearchPhrase, enteredLetters: EnteredLetters) =
    match phrase with
    | Cons(letter, tail) ->
        if ListContains(enteredLetters, letter) then
            printf "%c" letter
        elif letter = ' ' then
            printf " "
        else 
            printf "_"
        printPhrase'(state, tail, enteredLetters)
    | _ ->
        printfn "\n\n"

let printPhrase (input: InputState) = 
    match input with 
    | Correct (state, searchPhrase, enteredLetters, character) -> 
        printPhrase'(state, searchPhrase, enteredLetters) |> ignore
    | Incorrect (state, searchPhrase, enteredLetters, character) -> 
        printPhrase'(state, searchPhrase, enteredLetters) |> ignore
    | _ -> ()
    input