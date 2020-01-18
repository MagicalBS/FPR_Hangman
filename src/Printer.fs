module Printer

open System
open Domain


let printHangman (state: HangmanState) = 
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

let printWonOrLost (state: GameState) : GameState = 
    match state with
    | Won phrase -> 
        printfn "Congratulations, you won.\nPress enter to play again!"
        Console.ReadLine() |> ignore
        initGame
    | Lost phrase -> 
        printfn "Congratulations, you dead.\nPress enter to play again!" 
        Console.ReadLine() |> ignore
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

let printPhrase (state: HangmanState, phrase: SearchPhrase, enteredLetters: EnteredLetters) = 
    printf "Phrase to search: "
    printPhrase'(state, phrase, enteredLetters)

let printEnterSearchPhrase() =
    printfn "Enter the Search Phrase: "

let printGuessLetter() = 
    printfn "Guess a letter: "

let printCorrectLetter() = 
    printfn "This letter was correct!"

let printIncorrectLetter() = 
    printfn "This letter was incorrect! You suck!"

let printLetterAlreadyGuessed(character: char) = 
    printfn "This letter \'%c\' was already guessed! You suck!" character

let rec printList(letters: List<char>) = 
    match letters with
    | Cons(letter, tail) -> 
        match tail with 
        | Cons(letter', tail') ->
            printf "%c, " (System.Char.ToUpper(letter))
        | _ ->        
            printf "%c" (System.Char.ToUpper(letter))
        printList tail
    | _ ->
        printfn "\n"

let printEnteredLetters(enteredLetters: List<char>) = 
    printf "Entered letters: "
    printList(enteredLetters)

let printInvalidSearchPhrase(regex: string) = 
    printf "The search phase was invalid. Valid Letters: A-Z, a-z and a word has a maximum of 15 characters. 3 Words allowed." 

let clearAfterEnterIfNotOver(inputState: InputState) = 
    match inputState with 
    | Correct(Success, _, _, _) ->
         printf ""
    | Incorrect (Dead, _, _, _) -> 
        printHangman Dead
    | _ ->
        printfn "Hit Enter to continue"
        Console.ReadLine() |> ignore
        Console.Clear()
