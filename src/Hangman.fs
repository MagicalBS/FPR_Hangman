module Hangman

open System
open Domain
open System.Text.RegularExpressions

let initHangman(searchPhrase: SearchPhrase) =
    (Initial, searchPhrase, End)


let isValidSearchPhrase (input: string) = 
    Regex.IsMatch(input, "^([A-z]){1,15}(\\s{1}([A-z]){1,15}){0,2}$")

let readSearchPhrase (input: string) =
    Console.Clear()
    if isValidSearchPhrase input then
        input
        |> createSearchPhrase
        |> InProgress 
    else
        EnterSearchPhrase


let printHangman (state: HangmanState) = 
    match state with 
    | Initial ->
        System.IO.File.ReadAllText("HangmanAscii/Initial.txt")
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
    state


let rec printPhrase (state: HangmanState, phrase: SearchPhrase, enteredLetters: EnteredLetters) =
    match phrase with
    | Cons(letter, tail) ->
        if ListContains(enteredLetters, letter) then
            printf "%c" letter
        else
            printf "_"
        printPhrase(state, tail, enteredLetters)
    | _ ->
        printfn "\n\n"
        printHangman(state)


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


let rec hangmanLoop (state: HangmanState, searchPhrase: SearchPhrase, enteredLetters: EnteredLetters): GameState =
    match state with 
    | Dead ->
        Lost searchPhrase
    | _ ->
        Won searchPhrase
    // Console.ReadLine()
    // |> read
    // |> evaluate update state
    // |> print
    // |> loop


let playHangman (state: GameState) =
    match state with
    | InProgress searchPhrase ->
        initHangman searchPhrase 
        |> hangmanLoop 
    | _ ->
        state


let rec gameLoop (state: GameState) =
    printf "Enter the Search Phrase: "
    Console.ReadLine()
    |> readSearchPhrase
    |> playHangman
    |> printWonOrLost
    |> gameLoop