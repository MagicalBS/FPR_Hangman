module Domain


type List<'a> = 
    | End
    | Cons of 'a * List<'a>

type SearchPhrase = List<char>

type EnteredLetters = List<char>

let rec createSearchPhrase(input: string) : List<char> = 
    if input.Length > 1 then
        Cons(input.[0], createSearchPhrase(input.Substring(1)))
    else
        Cons(input.[0], End)

let rec ListContains (list: List<'a>, wanted: 'a): bool =
    match list with
    | Cons(current, tail) ->
        if current = wanted then
            true
        else
            ListContains(tail, wanted)
    | _ ->
        false

let rec WrongInputCount (phrase: SearchPhrase, letters: EnteredLetters): int = 
    match letters with
    | Cons(letter, tail) ->
        if ListContains(phrase, letter) then
            WrongInputCount(phrase, tail)
        else
            WrongInputCount(phrase, tail) + 1        
    | _ ->
        0

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