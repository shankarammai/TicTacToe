// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace TicTacToe
open System
open vsPc
open vsHuman
open GameCore

module Main=
    let rec mainmenu () =
        printfn "Please Select a option "
        printfn "1. Play Against Human"
        printfn "2. Play Against PC "
        printfn "3. View Leaderboard"
        printfn "4. Quit"
        printfn ""
        printf "Enter the Option >> "

        let useroptioninput = Console.ReadLine()
        match System.Int32.TryParse useroptioninput with
        | (true, number) when number = 1
                         -> loadAgainstHuman()
                            printf"Press any key to continue . . . "
                            Console.ReadKey()|> ignore
                            Console.Clear()
                            mainmenu()
        | (true, number) when number = 2
                         -> loadAgainstPc()
                            printf"Press any key to continue . . . "
                            Console.ReadKey()|> ignore
                            Console.Clear()
                            Console.Clear()
                            mainmenu()
        | (true, number) when number = 3
                         -> viewLeaderBoard()
                            printf"Press any key to continue . . .  "
                            Console.ReadKey()|> ignore
                            Console.Clear()
                            mainmenu()
        | (true, number) when number = 4
                         -> 0 //end game
        | _              -> printfn "Please select the correct value \n"
                            mainmenu ()

    [<EntryPoint>]
    let main _ = 
        printfn ""
        printfn "Welcome To Tic Tac Toe Game"
        printfn ""
        mainmenu() |>ignore
        Console.ReadKey()|>ignore
        0 // return an integer exit code
    
    
    

