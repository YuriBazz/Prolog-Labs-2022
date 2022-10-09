:- initialization(consult(`kinship.pro`)).
%Задача 1

pred(X, Y) :- parent(X, Y).
pred(X, Y) :- setof(C, (parent(X, C), pred(C, Y)), _).

% Задача 2

brother(X, Y):-
	male(X), parent(Z1, X), male(Z1), parent(Z2, X), female(Z2), parent(Z1, Y), parent(Z2, Y),  Y \= X.

% Задача 3

married(X, Y):-
	setof(Z, (parent(X, Z), parent(Y, Z), X \= Y), _).

% Задача 4 

husband(X, Y):-
	male(X), married(X, Y).

% Задача 5

siblings(X, Y):-
	parent(Z1, X), male(Z1), parent(Z2, X), female(Z2), parent(Z1, Y), parent(Z2, Y), X \= Y.
cousin(X, Y):-
	parent(Z, X), siblings(Z, T), parent(T, Y).

% Задача 6

num_of_children(X, N):-
	setof(Ch, (parent(Z, X), male(Z), \+ (parent(X,_)), Ch='!' ; parent(X,Ch)), Chs),
	(
		member('!', Chs), N = 0
		;
		\+ (member('!', Chs)), length(Chs, N)
	).

% Задача 7

nephews(Lst, Y) :- 
	(
		male(Y) 
		;
		female(Y)
	), findall(C, (siblings(Y, X), parent(X, C)), Lst1),sort(Lst1, Lst).

% Задача 8

family([H,W|Chs]) :- setof(Ch, (male(H), female(W), parent(H, Ch), parent(W, Ch)), Chs).