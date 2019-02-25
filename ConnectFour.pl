% Assignment 3, 159.202, 2017 S2
% Clancy, Isaac, 16125296,
% this program is a simple texted based connect 4 game for two players on the same
% computer.
% All inputs must end in a full stop

% main function used to play one or more games of connect4
connect4 :- prompt(_, ''), repeat, run, not(shouldRestartOrQuit()).
	
shouldRestartOrQuit() :- nl, repeat, write("Do you want to (r)estart or (q)uit?: "), 
	catch(read(Input), _, fail), handleExitRestart(Input).
handleExitRestart(q) :- halt.
handleExitRestart(r).

% plays one game of connect4
run :- write('\e[H\e[2J'), createBoard(Board), writeBoard(Board),
	mainLoop(red, Board, 42, continue), !.

createBoard(Board) :- Board = [[' ',' ',' ',' ',' ',' ',' '],
	[' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' '],
	[' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' '],
	[' ',' ',' ',' ',' ',' ',' ']].
	
mainLoop(Player, Board, FreeSlots, continue) :- NewFreeSlots is FreeSlots - 1,
	handleTurn(Player, Board, NewBoard, NewFreeSlots, NewGameState), 
	nextPlayer(Player, NewPlayer), 
	mainLoop(NewPlayer, NewBoard, NewFreeSlots, NewGameState).
mainLoop(Player, _, _, GameState) :- GameState \= continue, gameOver(Player, GameState).

nextPlayer(red, blue).
nextPlayer(blue, red).
writeToken(red) :- ansi_format([fg(red)], 'o', []).
writeToken(blue) :- ansi_format([fg(blue)], 'x', []).
	
handleTurn(Player, Board, NewBoard, FreeSlots, GameState) :- repeat,
	write("Enter column for "), write(Player), write(" player: "),
	catch(read(Input), _, fail), integer(Input), X is Input - 1, 
	setCoord(Player, X, Y, Board, NewBoard), write('\e[H\e[2J'), writeBoard(NewBoard),
	getGameState(X, Y, NewBoard, FreeSlots, GameState), !.
	
getGameState(_, Y, Board, _, win) :- horizonalWin(Y, Board), !.
getGameState(X, _, Board, _, win) :- verticalWin(X, Board), !.
getGameState(X, Y, Board, _, win) :- diagonalWin(X, Y, Board), !.
getGameState(_, _, _, 0, draw) :- !.
getGameState(_, _, _, _, continue).
	
horizonalWin(Y, Board) :- nth0(Y, Board, Row), fourInARow(Row).

verticalWin(X, Board) :- buildVerticalList(X, Board, Column), fourInARow(Column).

buildVerticalList(X, [Row1 | T1], [H | T2]) :- nth0(X, Row1, H),
	buildVerticalList(X, T1, T2).
buildVerticalList(_, [], []).

diagonalWin(X, Y, Board) :- Min is min(X, Y), StartX is X - Min, StartY is Y - Min,
	buildDiagonalList(StartX, StartY, Board, List, 1), fourInARow(List).
diagonalWin(X, Y, Board) :- X1 is 6 - X, Min is min(X1, Y), StartX is X + Min,
	StartY is Y - Min, buildDiagonalList(StartX, StartY, Board, List, -1),
	fourInARow(List).

buildDiagonalList(X, Y, Board, List, Value) :- length(StartList, Y),
	append(StartList, EndList, Board),
	buildDiagonalListHelper(X, Y, EndList, List, Value), !.
buildDiagonalListHelper(X, Y, [Row | T1], List, 1) :- X < 7, Y < 6, nth0(X, Row, H),
	NewX is X + 1, NewY is Y + 1, buildDiagonalListHelper(NewX, NewY, T1, T2, 1),
	List = [H | T2].
buildDiagonalListHelper(X, Y, [Row | T1], List, -1) :- X >= 0, Y < 6, nth0(X, Row, H),
	NewX is X - 1, NewY is Y + 1, buildDiagonalListHelper(NewX, NewY, T1, T2, -1),
	List = [H | T2].
buildDiagonalListHelper(_, _, _, [], _).

fourInARow([H | T]) :- fourInARow(T, H, 1).
fourInARow(_, _, 4).
fourInARow([H | T], E, _) :- H \= E, fourInARow(T, H, 1).
fourInARow([' ' | T], ' ', _) :- fourInARow(T).
fourInARow([H | T], H, Count) :- H \= ' ', NewCount is Count + 1,
	fourInARow(T, H, NewCount).

writeBoard([]).
writeBoard([H | T]) :- put('|'), writeRow(H), nl, writeBoard(T).

writeRow([]).
writeRow([' ' | T]) :- write(" |"), writeRow(T), !.
writeRow([H | T]) :- writeToken(H), put('|'), writeRow(T).

setCoord(Char, X, Y, Board, NewBoard) :- setCoord(Char, X, Y, 5, Board, NewBoard).
setCoord(Char, X, Y, Y, Board, NewBoard) :- nth0(Y, Board, Row), nth0(X, Row, Element),
	Element = ' ', replace(X, Row, Char, NewRow), replace(Y, Board, NewRow, NewBoard).
	
setCoord(Char, X, FinalY, Y, Board, NewBoard) :- nth0(Y, Board, Row), 
	nth0(X, Row, Element),	Element \= ' ', NewY is Y - 1, NewY >= 0,
	setCoord(Char, X, FinalY, NewY, Board, NewBoard).

replace(0, [_ | T], Item, [Item | T]).
replace(Index, [H | T], Item, NewList) :- Index > 0, NewIndex is Index - 1,
	replace(NewIndex, T, Item, NewTail), NewList = [H | NewTail].

% first parameter is player for next turn after win
gameOver(Player, win) :- nextPlayer(OldPlayer, Player), write(OldPlayer),
	write(" player wins").
gameOver(_, draw) :- write("draw!").

:- connect4.