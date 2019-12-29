module Hangman

open System
open Domain
open System.Text.RegularExpressions

let initHangman(searchPhrase: SearchPhrase) =
    (Initial, searchPhrase, End)


let isValidSearchPhrase (input: string) = 
    Regex.IsMatch(input, "^([A-z]){1,15}(\\s{1}([A-z]){1,15}){0,2}$")

let readSearchPhrase (input: string) =
    // Console.Clear()
    if isValidSearchPhrase input then
        input
        |> createSearchPhrase
        |> InProgress 
    else
        EnterSearchPhrase




type SuccessValue = HangmanState * SearchPhrase * EnteredLetters * char
type ErrorValue = EnteredLetters

type InputState =
    | Unverified of SuccessValue
    | Correct of SuccessValue
    | Incorrect of SuccessValue
    | Invalid of ErrorValue



let verifyLetterInput(input: char, state: HangmanState, searchPhrase: SearchPhrase, enteredLetters: EnteredLetters): InputState = 
    if ListContains(enteredLetters, input) && Regex.IsMatch((string)input, "^[A-z]$") then
        Unverified (state, searchPhrase, enteredLetters, input)
    else 
        Invalid enteredLetters


let enhanceEnteredLetters(result: InputState) = 
    match result with
    | Unverified (state, searchPhrase, enteredLetters, character) -> 
        if ListContains(searchPhrase, character) then
            Correct (state, searchPhrase, Cons(character, enteredLetters), character)
        else 
            Incorrect (state, searchPhrase, Cons(character, enteredLetters), character)

    | Invalid enteredLetters -> 
        Invalid enteredLetters 
        
    | _ -> 
        failwith "Invalid State"


let enhanceState(result: InputState) =
    match result with 
    | Unverified _ -> 
        failwith "Invalid State"
    | Correct (state, searchPhrase, enteredLetters, character) ->  
        Correct (state, searchPhrase, enteredLetters, character)
    | Incorrect (state, searchPhrase, enteredLetters, character) -> 
        match state with 
        | Initial ->
            Incorrect (Gallows, searchPhrase, enteredLetters, character)
        | Gallows ->
            Incorrect (Rope, searchPhrase, enteredLetters, character)
        | Rope ->
            Incorrect (Head, searchPhrase, enteredLetters, character)
        | Head ->
            Incorrect (Arms, searchPhrase, enteredLetters, character)
        | Arms ->
            Incorrect (Body, searchPhrase, enteredLetters, character)
        | Body ->
            Incorrect (Legs, searchPhrase, enteredLetters, character)
        | Legs ->
            Incorrect (Dead, searchPhrase, enteredLetters, character)
        | _ ->
            failwith "Invalid State"
    | Invalid err -> 
        Invalid err
        

let rec checkWinCondition (searchPhrase: SearchPhrase, enteredLetters: EnteredLetters): bool =
    match searchPhrase with
    | Cons(letter, tail) ->
        if ListContains(enteredLetters, letter) then
            checkWinCondition(tail, enteredLetters)
        else
            false
    | _ ->  
        true

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

let rec checkWonOrLost (result: InputState) =
    match result with
    | Unverified _ ->
        failwith "Invalid State"
    | Correct (state, searchPhrase, enteredLetters, character) ->
        if checkWinCondition(searchPhrase, enteredLetters) then
            Correct(Success, searchPhrase, enteredLetters, character)
        else
            Correct(state, searchPhrase, enteredLetters, character)
    | Incorrect (state, searchPhrase, enteredLetters, character) ->
        match state with 
        | Dead -> 
            Incorrect (state, searchPhrase, enteredLetters, character)
        | _ -> 
            Incorrect (state, searchPhrase, enteredLetters, character)
    | Invalid err ->
        Invalid err


let rec hangmanLoop (state: HangmanState, searchPhrase: SearchPhrase, enteredLetters: EnteredLetters): GameState =
    match state with 
    | Dead ->
        Lost searchPhrase
    | Success -> 
        Won searchPhrase
    | _ ->
        let inputState = 
            (Console.ReadKey().KeyChar, state, searchPhrase, enteredLetters)
            |> verifyLetterInput
            |> enhanceEnteredLetters
            |> enhanceState
            |> printHangman
            |> checkWonOrLost

        match inputState with
        | Unverified _ ->
            failwith "Invalid State"
        | Correct (state', searchPhrase', enteredLetters', character) ->
            hangmanLoop (state', searchPhrase', enteredLetters')
        | Incorrect (state', searchPhrase', enteredLetters', character) ->
            hangmanLoop (state', searchPhrase', enteredLetters')
        | Invalid err ->
            hangmanLoop (state, searchPhrase, enteredLetters)


let playHangman (state: GameState) =
    match state with
    | InProgress searchPhrase ->
        initHangman searchPhrase 
        |> hangmanLoop 
    | _ ->
        state

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


let rec gameLoop (state: GameState) =
    printf "Enter the Search Phrase: "
    Console.ReadLine()
    |> readSearchPhrase
    |> playHangman
    |> printWonOrLost
    |> gameLoop

let rec printPhrase' (state: HangmanState, phrase: SearchPhrase, enteredLetters: EnteredLetters) =
    match phrase with
    | Cons(letter, tail) ->
        if ListContains(enteredLetters, letter) then
            printf "%c" letter
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

    printHangman(input)
