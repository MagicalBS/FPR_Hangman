module Domain


type List<'a> = 
    | End
    | Cons of 'a * List<'a>

type SearchPhrase = List<char>

type EnteredLetters = List<char>

let rec createSearchPhrase(input: string) : List<char> = 
    if input.Length > 1 then
        Cons(input.[input.Length - 1], createSearchPhrase(input.Substring(0, input.Length - 1)))
    else
        Cons(input.[0], End)

let rec ListContains (list: List<char>, wanted: char): bool =
    match list with
    | Cons(current, tail) ->
        
        if (current.GetType() = typedefof<char>) then
            if System.Char.ToUpper(current) = System.Char.ToUpper(wanted) then
                true
            else
                ListContains(tail, wanted)
        else         
            if current = wanted then
                true
            else
                ListContains(tail, wanted)
    | _ ->
        false

type HangmanState = 
    | Initial
    | Gallows
    | Rope
    | Head
    | Arms
    | Body
    | Legs
    | Dead
    | Success

type GameState = 
    | EnterSearchPhrase
    | InProgress of SearchPhrase
    | Won of SearchPhrase
    | Lost of SearchPhrase

type GuessedLetter = char

let initGame: GameState =
    EnterSearchPhrase

type SuccessValue = HangmanState * SearchPhrase * EnteredLetters * char
type ErrorValue = EnteredLetters
type AlreadyGuessedValue = char

type InputState =
    | Unverified of SuccessValue
    | Correct of SuccessValue
    | Incorrect of SuccessValue
    | AlreadyGuessed of AlreadyGuessedValue
    | Invalid of ErrorValue
