% Задача 1 

map(_, [], _, []).
map([], _, _, []).
map([H|T], [H1|T1], F, [Hres|Tres]) :-
	Term =.. [F, H, H1, Hres], call(Term),
	map(T, T1, F, Tres).
add(A, B, C) :- C is A + B.

% Задача 2

choosePairs(L, L1, Pr, Lres) :-
	cPi(L, L1, Pr, X),
	(
		var(Lres) -> X = Lres
		;
		sort(X,R1),sort(Lres,R1)
	).	
	
cPi([], [], _, []).
cPi([H|T], [H1|T1], Pr, Lst) :-
	Term =.. [Pr, H, H1],
	call(Term) ->
		Hres = H - H1, cPi(T, T1, Pr, Tres), Lst=[Hres|Tres]
		;
		cPi(T, T1, Pr, Lst).

% Задача 3

foldl(X, _, [], X).
foldl(X0, F, [H|T], Lst) :-
	Term=.. [F, X0, H, X1],
	call(Term),
	foldl(X1, F, T, Lst).
	
% Задача 4
%insert из 2 практики

insert(tr(X, L, R), X, tr(X, L, R)). % т.к. не оговорено что делать при равенстве корня и введенного числа, сохраняем число в корне
insert(nil, X, T) :- T = tr(X, nil, nil).
insert(tr(X, L, R), N, T) :-
	X > N, L = nil,
	NL = tr(N, nil,nil),
	T = tr(X, NL, R)
	;
	X < N, R = nil,
	NR = tr(N, nil,nil),
	T = tr(X, L, NR).
insert(tr(X, L, R), N, tr(NX, NL, NR)) :-
	X > N, \+ L = nil,
	insert(L, N, L1),
	tr(NX, NL, NR) = tr(X, L1, R)
	;
	X < N, \+ R = nil,
	insert(R, N, R1),
	tr(NX, NL, NR) = tr(X, L, R1).
	
treeFromList([H|T], Tree) :-
	foldl(tr(H, nil, nil), insert, T, Tree).
	
	
% Задача 5

addHead(_, [], []).
addHead(X, [H|T], [[X|H]|Tres]) :-
	addHead(X,T,Tres).
	
