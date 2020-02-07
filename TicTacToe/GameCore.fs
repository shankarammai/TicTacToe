namespace TicTacToe
open System
module GameCore=
            
        // Box can be Either X O or Empty 
        type BoxValue=
            |X
            |O
            |Emptybox

        // Position of the box
        type Boxposition=
            |A |B |C
            |D |E |F
            |G |H |I
            |Invalidposition
        // Game can be either Draw and Won 
        type GameStatus=
            |Draw
            |Won

        // Board can be of 9 BoxValues 
        type Board=
            |BoxValue of BoxValue * BoxValue  * BoxValue * BoxValue  * BoxValue * BoxValue  * BoxValue * BoxValue  * BoxValue

        // This function will be used while printing the board. Board cannot have Emptybox inside it. It should be replaced by blank space.
        let changeValue =function
            | Emptybox -> " "
            | X -> "X"
            | O -> "O"
        //This function gets the value of the position of the board, given board and position are provided.
        let getValueOfPosition board position=
            match board, position with
                |(valueofposition,_,_,_,_,_,_,_,_),A->valueofposition
                |(_,valueofposition,_,_,_,_,_,_,_),B->valueofposition
                |(_,_,valueofposition,_,_,_,_,_,_),C->valueofposition
                |(_,_,_,valueofposition,_,_,_,_,_),D->valueofposition
                |(_,_,_,_,valueofposition,_,_,_,_),E->valueofposition
                |(_,_,_,_,_,valueofposition,_,_,_),F->valueofposition
                |(_,_,_,_,_,_,valueofposition,_,_),G->valueofposition
                |(_,_,_,_,_,_,_,valueofposition,_),H->valueofposition
                |(_,_,_,_,_,_,_,_,valueofposition),I->valueofposition
        
        //These are all the positions on the board.
        let allBoxes=
            List.ofSeq[A;B;C;D;E;F;G;H;I]

        //This function calculates all the remaining empty spaces on the board.
        let remainingEmptyBoxes board=
            let trueAndFalse=List.countBy(( = )Emptybox << getValueOfPosition board ) allBoxes
            match trueAndFalse with 
            |[(true,value)]->value
            |[(true,value);(false,_)]->value
            |[(false,_);(true,value)]->value
            |[(false,_)]->0
        // Initially board is empty.
        let emptyBoard=
                    ((Emptybox),(Emptybox),(Emptybox),
                    (Emptybox),(Emptybox),(Emptybox),
                    (Emptybox),(Emptybox),(Emptybox))
           

        //This function display the board in a better, given a board is given
        let  showBoard (valueofa , valueofb,valueofc , valueofd,valueofe , valueoff,valueofg , valueofh,valueofi)=
            printfn """
                        %s |%s |%s
                        -------
                        %s |%s |%s
                        -------
                        %s |%s |%s""" 
                
                        (changeValue valueofa) (changeValue valueofb) (changeValue valueofc)
                        (changeValue valueofd) (changeValue valueofe) (changeValue valueoff)
                        (changeValue valueofg) (changeValue valueofh) (changeValue valueofi)
   
         // This function takes string and convert it to Boxpostion
        let parseUserPosition (userInput:String) =
                match userInput.Split[|','|] with 
                | [|row;column|]->
                    match row,column with
                    |("1","1")->A
                    |("1","2")->B
                    |("1","3")->C
                    |("2","1")->D
                    |("2","2")->E
                    |("2","3")->F
                    |("3","1")->G
                    |("3","2")->H
                    |("3","3")->I
                    |(_,_)-> Invalidposition

        //It checks if the game is Won by someone or not.
        let checkGameStatus board=
            match board with
                |(a,b,c,_,_,_,_,_,_) when a=b && b=c && a<>Emptybox ->(Won,a)
                |(_,_,_,d,e,f,_,_,_) when d=e && e=f && d<>Emptybox ->(Won,d)
                |(_,_,_,_,_,_,g,h,i) when g=h && h=i && g<>Emptybox ->(Won,g)
                |(a,_,_,d,_,_,g,_,_) when a=d && d=g && a<>Emptybox->(Won,a)
                |(_,b,_,_,e,_,_,h,_) when b=e && e=h && b<>Emptybox->(Won,b)
                |(_,_,c,_,_,f,_,_,i) when c=f && f=i && c<>Emptybox->(Won,c)
                |(a,_,_,_,e,_,_,_,i) when a=e && e=i && a<>Emptybox->(Won,a)
                |(_,_,c,_,e,_,g,_,_) when c=e && e=g && c<>Emptybox->(Won,c)
                |(_) ->(Draw,Emptybox)

        //It checks if the box the user wants to fill is empty or not
        let checkIfEmpty board position =
            match board, position with
                |(Emptybox,_,_,_,_,_,_,_,_),A->true
                |(_,Emptybox,_,_,_,_,_,_,_),B->true
                |(_,_,Emptybox,_,_,_,_,_,_),C->true
                |(_,_,_,Emptybox,_,_,_,_,_),D->true
                |(_,_,_,_,Emptybox,_,_,_,_),E->true
                |(_,_,_,_,_,Emptybox,_,_,_),F->true
                |(_,_,_,_,_,_,Emptybox,_,_),G->true
                |(_,_,_,_,_,_,_,Emptybox,_),H->true
                |(_,_,_,_,_,_,_,_,Emptybox),I->true
                |_ ->false

        // This function knows which value to mark in the board
        let markToMake mark = 
            match mark with
            |X -> O
            |O ->X

         //This function fills the board and gives a new board.
        let markOnBoard board position value=
            match board,position with
            |(_,b,c,d,e,f,g,h,i),A->(value,b,c,d,e,f,g,h,i)
            |(a,_,c,d,e,f,g,h,i),B->(a,value,c,d,e,f,g,h,i)
            |(a,b,_,d,e,f,g,h,i),C->(a,b,value,d,e,f,g,h,i)
            |(a,b,c,_,e,f,g,h,i),D->(a,b,c,value,e,f,g,h,i)
            |(a,b,c,d,_,f,g,h,i),E->(a,b,c,d,value,f,g,h,i)
            |(a,b,c,d,e,_,g,h,i),F->(a,b,c,d,e,value,g,h,i)
            |(a,b,c,d,e,f,_,h,i),G->(a,b,c,d,e,f,value,h,i)
            |(a,b,c,d,e,f,g,_,i),H->(a,b,c,d,e,f,g,value,i)
            |(a,b,c,d,e,f,g,h,_),I->(a,b,c,d,e,f,g,h,value)
            |(a,b,c,d,e,f,g,h,i),Invalidposition->(a,b,c,d,e,f,g,h,i)


        //Needs proper implementation of leaderbord.
          // Loading Leaderbboard 
        let loadLeaderBoard()=
            System.IO.File.ReadLines("leaderboard.txt")
        let leaderboardToList()=
            loadLeaderBoard() |> Seq.map(fun x -> (x.Split(','))) |> Seq.map (fun x -> x.[0],(x.[1]))
        //let allscores=loadLeaderBoard() |> Seq.map(fun x -> (x.Split(','))) |> Seq.map (fun x -> x.[1])
        //let allNames=loadLeaderBoard() |> Seq.map(fun x -> (x.Split(','))) |> Seq.map (fun x -> x.[0])
        let viewLeaderBoard()=
            leaderboardToList() |> Seq.iter(fun x -> printfn  "    %s->%s" (fst x) (snd x))

        // This function is to update the leaderboard once the game is won by someone.
        //This feature is not properly implemeented
        let addToLeaderBoard  playername=
            //proper implementation needed
            System.IO.File.AppendAllText("leaderboard.txt","\n"+playername+",1")
