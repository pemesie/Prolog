:- module( helpers,
	 [ readMazeGame/3
	 , printMazeGame/1
	 ]
    ).

dropLast([_],[]).
dropLast([H|T],[H|R]):-
    dropLast(T,R).

readMazeGame(File,Moves,Maze):-
    open(File,read,Input),
    read(Input,Moves),
    readMaze(Input,Temp),

    %printMazeGame(Temp),

    dropLast(Temp,Maze),

   % printMazeGame(Temp),

    close(Input).

readMaze(Input,[]):-
    at_end_of_stream(Input),
    !.
readMaze(Input,[Row|Rows]):-
    \+ at_end_of_stream(Input),
    read(Input,Row),
    readMaze(Input,Rows).

printMazeGame([]).
printMazeGame([Row|Rows]):-
    writeln(Row),
    printMazeGame(Rows).

