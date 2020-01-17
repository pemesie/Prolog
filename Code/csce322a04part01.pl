
colsAndPlayers([Row|Rows]):-
   findNbers([Row|Rows], ListOfNumber),
   getMax(ListOfNumber, Max),
   length(Row, L),
   X is mod(L, 2), %writeln(X),
   Y is mod(Max, 2),%writeln(Y),
   X == Y.

getMax([M],M).

getMax([Head|Tail],Max) :- getMax(Tail,Max), Max >=  Head.

getMax([Head|Tail],Head) :-  getMax(Tail,Max), Head > Max.

f([],[]).
f([Head|Tail], Rest):-
    not(number(Head) ),
     f(Tail,Rest).
      %Numbers = Rest.

f([Head|Tail], Numbers):-
    number(Head),
    f(Tail,Rest),
    Numbers = [Head|Rest].

findNbers([],[]).

findNbers([Row|Rows],Numbers1):-
    f(Row, Nbers),
    findNbers(Rows,Numbers2),
    append(Nbers, Numbers2, Numbers1).
