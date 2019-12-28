type List<'a> = 
    | End
    | Cons of 'a * List<'a>

type SearchPhrase = string

type EnteredLetters = List<char>

let rec WrongInputCount (phrase: SearchPhrase, letters: EnteredLetters): int = 
    match letters with
    | Cons(letter, tail) ->
        if phrase.Contains((string)letter) then
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