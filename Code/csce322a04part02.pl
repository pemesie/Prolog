
fewestSlides(Maze,Moves):-
    %printMazeGame(Maze),
    %writeln('moves'),
    bfs(Maze,Moves),!.
    %bfs(Maze,Moves),!, writeln(Moves),length(Moves, NewMoves), bfs(Maze, NewMoves), writeln(NewMoves).


% Breath First Search
bfs(Maze, []):-
    \+(find2D(Maze, g,X)).

bfs(Maze, [S|M]):-
    between(0,10,Length),
    length(M,Length),
    move(Maze, NewMaze, S),
    bfs(NewMaze, M).
    %Moves is [S|M].


% find(List,What,Where)
find([What|_],What,1).
find([_|T],What,Where):-
	find(T,What,WhereT),
	Where is WhereT + 1.

find2D( [Row|_], What, (1,C) ):-
	find(Row,What,C).

find2D( [_|Rows], What, (R, RowsC) ):-
	find2D(Rows, What, (RowsR, RowsC)),
	R is RowsR + 1.

% set(Before,NewElement,Position,After)
set([_|T],NewElement,1,[NewElement|T]).
set([H|T],NewElement,P,[H|NewT]):-
	length([H|T],Len),
	between(1,Len,P),
	PM1 is P - 1,
	set(T,NewElement,PM1,NewT).

% change2D([Row|Rows],NewElement,(R,C),Result)
set2D([Row|Rows],NewElement,(1,C),[NewRow|Rows]):-
   set(Row,NewElement,C,NewRow).

set2D([Row|Rows],NewElement,(R,C),[Row|Result]):-
   length([Row|Rows],NumRows),
   between(1,NumRows,R),
   RNew is R - 1,
   set2D(Rows,NewElement,(RNew,C),Result).

%---------------------------------------------------
checkAhead(MazeBefore,Ro, Col,What):-
    find2D(MazeBefore,What,(Ro,Col)).
%---------------------------------------------------
 %%%%%%%%%%%%%%%%  MOVE LEFT  %%%%%%%%%%%%%%%%%%%%%%%
move(MazeBefore, MazeAfter1, l):-
    edge(MazeBefore, MazeAfter1, l),
    not(MazeAfter1=MazeBefore).
%%%%%%%%%%%%%%%%  MOVE DOWN  %%%%%%%%%%%%%%%%%%
move(MazeBefore, MazeAfter, d):-
    edge(MazeBefore, MazeAfter, d),
    not(MazeAfter=MazeBefore).
%%%%%%%%%%%%%% MOVE RIGHT %%%%%%%%%%%%%%%%%%%%%%
move(MazeBefore, MazeAfter, r):-
    edge(MazeBefore, MazeAfter, r),
    not(MazeAfter=MazeBefore).
%%%%%%%%%%%  MOVE UP  %%%%%%%%%%%%%%%%%%%%%%
move(MazeBefore, MazeAfter, u):-
    edge(MazeBefore, MazeAfter, u),
    not(MazeAfter=MazeBefore).

%-----------------------------------------------------------

%%%%%%%%%%%%%%%%%  MOVE LEFT  %%%%%%%%%%%%%%%%%%%%%%%

edge(MazeBefore, MazeAfter1, l):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Col2 is Col1 - 1,
    checkAhead(MazeBefore,Ro1, Col2,What),
    What == -, %writeln('a head '), writeln(What),writeln(Col2),printMazeGame(MazeBefore),
    set2D(MazeBefore, 1, (Ro1,Col2), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter),
    edge(MazeAfter, MazeAfter1, l).%,writeln('l '),printMazeGame(MazeAfter1).

edge(MazeBefore, MazeAfter, l):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Col2 is Col1 - 1,
    checkAhead(MazeBefore,Ro1, Col2,What),
    What == g,
    set2D(MazeBefore, 1, (Ro1,Col2), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter).

edge(MazeBefore, MazeAfter, l):-
    find2D(MazeBefore,1,(Ro1,Col1)),!,
    Col2 is Col1 - 1,
    checkAhead(MazeBefore,Ro1, Col2,What),
    What == x, %write('a head '), writeln(What),writeln(Col2),writeln('l '),printMazeGame(MazeBefore),
    MazeAfter = MazeBefore.

    %%%%%%%%%%%%%%%%  MOVE DOWN  %%%%%%%%%%%%%%%%%%
edge(MazeBefore, MazeAfter1, d):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Ro2 is Ro1 + 1,
    checkAhead(MazeBefore,Ro2, Col1,What),
    What == -,
    set2D(MazeBefore, 1, (Ro2,Col1), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter),
    edge(MazeAfter,  MazeAfter1, d).%,writeln('d '),printMazeGame(MazeAfter1).


edge(MazeBefore, MazeAfter, d):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Ro2 is Ro1 + 1,
    checkAhead(MazeBefore,Ro2, Col1,What),
    What == g,
    set2D(MazeBefore, 1, (Ro2,Col1), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter).

edge(MazeBefore, MazeAfter, d):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Ro2 is Ro1 + 1,
    checkAhead(MazeBefore,Ro2, Col1,What),
    What == x,
    MazeAfter = MazeBefore.

%%%%%%%%%%%%%% MOVE RIGHT %%%%%%%%%%%%%%%%%%%%%%
edge(MazeBefore,  MazeAfter1,  r):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Col2 is Col1 + 1,
    checkAhead(MazeBefore,Ro1, Col2,What),
    What == -,
    set2D(MazeBefore, 1, (Ro1,Col2), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter),
    edge(MazeAfter,  MazeAfter1, r).%,writeln('r '),printMazeGame(MazeAfter1).

edge(MazeBefore,  MazeAfter,  r):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Col2 is Col1 + 1,
    checkAhead(MazeBefore,Ro1, Col2,What),
    What == g,
    set2D(MazeBefore, 1, (Ro1,Col2), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter).

edge(MazeBefore, MazeAfter, r):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Col2 is Col1 + 1,
    checkAhead(MazeBefore,Ro1, Col2,What),
    What == x,
    MazeAfter = MazeBefore.
%%%%%%%%%%%  MOVE UP  %%%%%%%%%%%%%%%%%%%%%%
edge(MazeBefore, MazeAfter1, u):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Ro2 is Ro1 - 1,
    checkAhead(MazeBefore,Ro2, Col1,What),
    What == -,
    set2D(MazeBefore, 1, (Ro2,Col1), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter),
    edge(MazeAfter, MazeAfter1, u).%,writeln('u '),printMazeGame(MazeAfter1).

edge(MazeBefore, MazeAfter, u):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Ro2 is Ro1 - 1,
    checkAhead(MazeBefore,Ro2, Col1,What),
    What == g,
    set2D(MazeBefore, 1, (Ro2,Col1), TempMaze),
    set2D(TempMaze, -, (Ro1,Col1), MazeAfter).

edge(MazeBefore, MazeAfter, u):-
    find2D(MazeBefore,1,(Ro1,Col1)),
    Ro2 is Ro1 - 1,
    checkAhead(MazeBefore,Ro2, Col1,What),
    What == x,
    MazeAfter = MazeBefore.
