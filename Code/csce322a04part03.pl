
% Using Breath First Searth to solve this problem.
% Node is the state
% Edge is the direction: up, right, down, left.

printSequences([]).
printSequences([Sequence|Sequences]):-
    writeln(Sequence),
    printSequences(Sequences).

loadHelpers:-
	['helpers'],
	['csce322a04part01'],
	['csce322a04part02'],
	['csce322a04part03'],
	['csce322a04part04'].

part02:-
    readMazeGame('part02test01.scm',_,Game),
    printMazeGame(Game),goalWall(Game),!.




goalWall(Maze):-
    find2D(Maze,g,(Ro,Col)),
    checkAdjancantCell(Maze,(Ro,Col), Moves).

%--------------------------------------------------
checkAdjancantCell(Maze,(Ro1,Col), up):-
    Ro2 is Ro1 - 1,
    find2D(Maze,What,(Ro2,Col)),
    What == x.

checkAdjancantCell(Maze,(Ro1,Col1), right):-
    Col2 is Col1 + 1,
    find2D(Maze,What,(Ro1,Col2)),
    What == x.

checkAdjancantCell(Maze,(Ro1,Col1), down):-
    Ro2 is Ro1 + 1,
    find2D(Maze,What,(Ro2,Col1)),
    What == x.

checkAdjancantCell(Maze,(Ro1,Col1), left):-
    Col2 is Col1 - 1,
    find2D(Maze,What,(Ro1,Col2)),
    What == x.

% ----------------- HELPING PREDICATES------------------

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
%-----------------------------------------------
