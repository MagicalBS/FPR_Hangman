[<EntryPoint>]
let main argv =
    printfn "Welcome to the Hangman-Man!"
    printfn "Press CTRL+C to stop the program.\n\n"

    let initialState = Domain.initGame
    Hangman.gameLoop initialState
    0 // return an integer exit code
