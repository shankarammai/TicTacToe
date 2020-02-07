namespace TicTacToe
open System
open GameCore
module vsHuman=

    // This function is responsible for handling the user inputs for the game. If the input provided is wrong it is handled in this function.
    let rec getUserInput board =
        printf "Please entry box position (example:1,2) >> "
        let getPosition=Console.ReadLine()
        if (getPosition.Split [| ',' |]).Length <> 2 then 
            printfn "Invalid format, try again.\n"
            getUserInput board
        else
            let choosedPosition=parseUserPosition getPosition
            let isboxempty=checkIfEmpty board choosedPosition
            if (choosedPosition) <> Invalidposition  && (isboxempty) then 
                choosedPosition
            else
                printfn "Wrong Input or Box Not Empty.\n"
                getUserInput board

    // this function is responsible for running the vsHuman 
    let rec gamePlayVsHuman board mark playersNames=
        let position=(getUserInput board) //asks user for the position
        let newBoard= (markOnBoard board position mark) //makes mark on the board and give new board
        let gameStatus=(checkGameStatus newBoard) //check is game is won by someone or not
        showBoard newBoard //displays the new board 
        printfn "Turn of %A" (markToMake mark) //prints whose turn
        if  fst gameStatus=Won && snd gameStatus=X then  // if game is won by player X show new board. Tell who won it.
            showBoard newBoard
            printfn "Game Won By %s" (fst playersNames)
            addToLeaderBoard (fst playersNames)  // add winning player to the leaderboard
        else if fst gameStatus=Won && snd gameStatus=O then
            showBoard newBoard
            printfn "Game Won By %s" (snd playersNames)
            addToLeaderBoard (snd playersNames)
        else if fst gameStatus=Draw && (remainingEmptyBoxes newBoard)>0 then // if gamestaus is draw and if boxes are empty then ask for input again
            gamePlayVsHuman newBoard (markToMake mark) playersNames
        else if fst gameStatus=Draw && (remainingEmptyBoxes newBoard)=0 then // if gamestatus is draw and boxes are empty then game is over
            printfn "Game Drawn"
     //this function will get names of the player 
    let getPlayersName()=
        printf "Enter Player 1 Name >> "
        let player1=Console.ReadLine()
        printf "Enter Player 2 Name >> "
        let player2= Console.ReadLine()
        (player1,player2)

    // this  function will do necessary things before actually loading the vsHuman.
    let loadAgainstHuman()=
        let playersNames=getPlayersName()
        showBoard emptyBoard
        gamePlayVsHuman emptyBoard X playersNames
