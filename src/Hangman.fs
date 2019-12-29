module Hangman

open System
open Domain
open Printer
open System.Text.RegularExpressions

let initHangman(searchPhrase: SearchPhrase) =
    (Initial, searchPhrase, End)


let isValidSearchPhrase (input: string) = 
    Regex.IsMatch(input, "^([A-z]){1,15}(\\s{1}([A-z]){1,15}){0,2}$")

let convertSearchPhrase (input: string) =
    if isValidSearchPhrase input then
        input
        |> createSearchPhrase
        |> InProgress 
    else
        EnterSearchPhrase




let verifyLetterInput(input: string, state: HangmanState, searchPhrase: SearchPhrase, enteredLetters: EnteredLetters): InputState = 
    if input.Length = 1 then
        if ListContains(enteredLetters, input.[0]) = false && Regex.IsMatch((string)input, "^[A-z]$") then
                Unverified (state, searchPhrase, enteredLetters, input.[0])
            else 
                Invalid enteredLetters
    else
        Invalid enteredLetters

let enhanceEnteredLetters(result: InputState) = 
    match result with
    | Unverified (state, searchPhrase, enteredLetters, character) -> 
        if ListContains(searchPhrase, character) then
            printfn "This letter was correct!"
            Correct (state, searchPhrase, Cons(character, enteredLetters), character)
        else 
            printfn "This letter was incorrect! You Suck!"
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
        if ListContains(enteredLetters, letter) || letter = ' ' then
            checkWinCondition(tail, enteredLetters)
        else
            false
    | _ ->  
        true


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
        printfn "Guess a letter: "

        let inputState = 
            (Console.ReadLine(), state, searchPhrase, enteredLetters)
            |> verifyLetterInput
            |> enhanceEnteredLetters
            |> enhanceState
            |> printHangman
            |> printPhrase
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

let rec readMask pw =
    let k = Console.ReadKey()
    match k.Key with
    | ConsoleKey.Enter -> pw
    | _ ->
        Console.Write "\b*"
        readMask (k.KeyChar::pw)

let readSearchPhrase = fun () ->
    readMask [] |> String.Concat


let rec gameLoop (state: GameState) =
    Console.Clear()
    printf "Enter the Search Phrase: "
    readSearchPhrase()
    |> convertSearchPhrase
    |> playHangman
    |> printWonOrLost 
    |> gameLoop 


