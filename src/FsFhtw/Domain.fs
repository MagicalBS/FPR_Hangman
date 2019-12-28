module Domain

type State = int

type Message =
    | Increment
    | Decrement
    | IncrementBy of int
    | DecrementBy of int

let init () : State =
    0

let update (msg : Message) (model : State) : State =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1
    | IncrementBy x -> model + x
    | DecrementBy x -> model - x

type List<'a> = 
    | End
    | Cons of 'a * List<'a>

type SearchPhrase = List<char>

type EnteredLetters = List<char>

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

type GameState = 
    | EnterSearchPhrase
    | InProgress
    | Won
    | Lost

type GuessedLetter = char