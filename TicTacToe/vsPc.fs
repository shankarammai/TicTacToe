namespace TicTacToe
open System
open TicTacToe
open GameCore
module vsPc=
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

    //While playing vsPC this function is responsible for choosing the best move according to the board given.
    let pcChoosePosition board =
        match board with
        //Try to Win first if you can

        //first row possible chances
        |(Emptybox,b,c,_,_,_,_,_,_) when b=c && b=O->A
        |(a,Emptybox,c,_,_,_,_,_,_) when a=c && a=O->B 
        |(a,b,Emptybox,_,_,_,_,_,_) when a=b && a=O->C

        //second row possible chances
        |(_,_,_,Emptybox,e,f,_,_,_) when e=f && e=O->D
        |(_,_,_,d,Emptybox,f,_,_,_) when d=f && d=O->E
        |(_,_,_,d,e,Emptybox,_,_,_) when d=e && d=O->F    

        // third row possible chances
        |(_,_,_,_,_,_,Emptybox,h,i) when h=i && h=O->G
        |(_,_,_,_,_,_,g,Emptybox,i) when g=i && g=O->H
        |(_,_,_,_,_,_,g,h,Emptybox) when g=h && g=O->I

        //first column possible chances
        |(Emptybox,_,_,d,_,_,g,_,_) when d=g && d=O->A
        |(a,_,_,Emptybox,_,_,g,_,_) when a=g && a=O->D
        |(a,_,_,d,_,_,_,Emptybox,_) when a=d && a=O->G

        //second column possible chances
        |(_,Emptybox,_,_,e,_,_,h,_) when e=h && e=O->B
        |(_,b,_,_,Emptybox,_,_,h,_) when b=h && b=O->E
        |(_,b,_,_,e,_,_,Emptybox,_) when b=e && b=O->H


        //third column possible chances
        |(_,_,Emptybox,_,_,f,_,_,i) when f=i && f=O->C
        |(_,_,c,_,_,Emptybox,_,_,i) when c=i && c=O->F
        |(_,_,c,_,_,f,_,_,Emptybox) when c=f && c=O->I
    
        //Diagional CEG possible chances

        |(_,_,Emptybox,_,e,_,g,_,_) when e=g && e=O->C
        |(_,_,c,_,Emptybox,_,g,_,_) when c=g && c=O->E
        |(_,_,c,_,e,_,Emptybox,_,_) when c=e && c=O->G
        //Diagional AEI possible chances
        |(Emptybox,_,_,_,e,_,_,_,i) when e=i && e=O->A
        |(a,_,_,_,Emptybox,_,_,_,i) when a=i && a=O->E
        |(a,_,_,_,e,_,_,_,Emptybox) when a=e && a=O->I

        /////Defensive moves starts here
        // first row possible chances
        |(Emptybox,b,c,_,_,_,_,_,_) when b=c && b=X->A
        |(a,Emptybox,c,_,_,_,_,_,_) when a=c && a=X->B 
        |(a,b,Emptybox,_,_,_,_,_,_) when a=b && a=X->C

        //second row possible chances
        |(_,_,_,Emptybox,e,f,_,_,_) when e=f && e=X->D
        |(_,_,_,d,Emptybox,f,_,_,_) when d=f && d=X->E
        |(_,_,_,d,e,Emptybox,_,_,_) when d=e && d=X->F    

        // third row possible chances
        |(_,_,_,_,_,_,Emptybox,h,i) when h=i && h=X->G
        |(_,_,_,_,_,_,g,Emptybox,i) when g=i && g=X->H
        |(_,_,_,_,_,_,g,h,Emptybox) when g=h && g=X->I

        //first column possible chances
        |(Emptybox,_,_,d,_,_,g,_,_) when d=g && d=X->A
        |(a,_,_,Emptybox,_,_,g,_,_) when a=g && a=X->D
        |(a,_,_,d,_,_,_,Emptybox,_) when a=d && a=X->G

        //second column possible chances
        |(_,Emptybox,_,_,e,_,_,h,_) when e=h && e=X->B
        |(_,b,_,_,Emptybox,_,_,h,_) when b=h && b=X->E
        |(_,b,_,_,e,_,_,Emptybox,_) when b=e && b=X->H


        //third column possible chances
        |(_,_,Emptybox,_,_,f,_,_,i) when f=i && f=X->C
        |(_,_,c,_,_,Emptybox,_,_,i) when c=i && c=X->F
        |(_,_,c,_,_,f,_,_,Emptybox) when c=f && c=X->I
    
        //diagonal CEG possible chances

        |(_,_,Emptybox,_,e,_,g,_,_) when e=g && e=X->C
        |(_,_,c,_,Emptybox,_,g,_,_) when c=g && c=X->E
        |(_,_,c,_,e,_,Emptybox,_,_) when c=e && c=X->G

        //diagonal AEI possible chances

        |(Emptybox,_,_,_,e,_,_,_,i) when e=i && e=X->A
        |(a,_,_,_,Emptybox,_,_,_,i) when a=i && a=X->E
        |(a,_,_,_,e,_,_,_,Emptybox) when a=e && a=X->I
   
        // all posible moves if you cant win the game or another user cant win the game.
        |(_,_,_,_,Emptybox,_,_,_,_)->E
        |(Emptybox,_,_,_,_,_,_,_,_)->A
        |(_,_,_,_,_,_,_,_,Emptybox)->I
        |(_,_,Emptybox,_,_,_,_,_,_)->C
        |(_,_,_,_,_,_,Emptybox,_,_)->G
        |(_,Emptybox,_,_,_,_,_,_,_)->B
        |(_,_,_,Emptybox,_,_,_,_,_)->D
        |(_,_,_,_,_,Emptybox,_,_,_)->F
        |(_,_,_,_,_,_,_,Emptybox,_)->H


    // this function is responsible for running vsPC feature
    let rec gamePlayVsPC board mark player1=
        showBoard board
        if mark=X then
            printfn "Turn of %A" mark
        else 
            printfn "Move Made by Pc"
        let checkTurn mark=  // cheking whose turn to make mark and geting position accordingly
            if mark = O then
                pcChoosePosition board
            else
                getUserInput board
                  
        let position=(checkTurn mark)
        let newBoard= (markOnBoard board position mark)
        let gameStatus=(checkGameStatus newBoard)    
        if  fst gameStatus=Won && snd gameStatus=X then // if game is won by player
            addToLeaderBoard (player1)  // add winning player to the leaderboard
            showBoard newBoard
            printfn "Game Won By %A" mark
        if  fst gameStatus=Won && snd gameStatus=O then // if game is won by Pc
            showBoard newBoard
            printfn "Game Won By %A" mark
            
        else if fst gameStatus=Draw && (remainingEmptyBoxes newBoard)>0 then // if the game is over
            if markToMake mark =X then 
                let pcBoard=(markOnBoard newBoard (pcChoosePosition board) mark)
                gamePlayVsPC pcBoard (markToMake mark) player1
                
            else
                gamePlayVsPC newBoard (markToMake mark) player1
        else if fst gameStatus=Draw && (remainingEmptyBoxes newBoard)=0 then
            printfn "Game Ended as Draw"

    // this  function will do necessary things before actually loading the vsPC.
    let loadAgainstPc()=
        let player1=
            printf "Enter Player 1 Name >> "
            Console.ReadLine()
        gamePlayVsPC emptyBoard X player1
