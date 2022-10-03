
% Задача 1

myReverse(L, X) :- helper1(L, [], X).
helper1([], X, X).
helper1([H|T], L1, L2) :- helper1(T, [H|L1], L2).

palindrome(L) :- helper_reg(L, X), myReverse(X, X).
a_to_A(H, H1) :- 
    H = 'A', !, H1 = 'a'
    ;
    H = 'B', !, H1 = 'b'
    ;
    H = 'C', !, H1 = 'c'   
    ;
    H1 = H.

helper_reg(L, X) :-
    L = [], !, X = []
    ;
    L = [H|T], a_to_A(H, H1), helper_reg(T, X1), X = [H1|X1].

% Задача 2

primeAnd1(1). 
primeAnd1(X) :- helper3(X,2).
helper3(N, D) :- (N =:= D) ; (N > D, N mod D =\= 0, D1 is D + 1, helper3(N, D1)).
deleteNonPrime([], []).
deleteNonPrime([H|T], X) :-
    primeAnd1(H), !, deleteNonPrime(T, X1), X = [H|X1]
    ;
    deleteNonPrime(T, X1), X = X1.

% Задача 3

myPrefix(L1,L2) :-
    L1 = []
    ;
    L1 =[H1|T1], L2 = [H2|T2], H1 = H2, myPrefix(T1, T2).

mySublist(L1, L2) :- 
    var(L1), var(L2),
    (
        L1 = L2
        ;
        L = [_|L1], mySublist(L, L2)
    ).
mySublist(L1, L2) :-
    myPrefix(L1, L2), !
    ;
    L2 = [_|T2], mySublist(L1, T2).

% Задача 4

myConfluens(L1, L2, X) :-
    L1 = [], L2 = [], !, X = []
    ;
    L1 = [H1|T1], L2 = [], !, myConfluens(T1, L2, X1), X = [H1|X1]
    ;
    L1 = [], L2 = [H2|T2], !, myConfluens(L1, T2, X1), X= [H2|X1]
    ;
    L1 = [H1|T1], L2 = [H2|T2], !,
    (
        H1 < H2, !, myConfluens(T1, L2, X1), X = [H1|X1]
        ;
        myConfluens(L1, T2, X1), X = [H2|X1]
    ).

% Задача 5

numlst(N, Lst):-
	var(N), list(Lst), iter(Lst, 0, X), X = N
    ;
	number(N), \+ (N = 0), helper5(N, [], Lst)
    ;
	var(N), var(Lst), f(1, N, Lst).
helper5(N, Lst, Lst1):-
	N =\= 0, N1 is N div 10, N2 is N mod 10, helper5(N1, [N2|Lst], Lst1).
helper5(0, Lst, Lst).
f(N, M, Lst):-
	M is N, helper5(N, [], Lst)
    ;
	N1 is N + 1, f(N1, M, Lst).
iter([A|B], C, D):-
	B = [], !, D is C * 10 + A
    ;
	C1 is C * 10 + A, iter(B, C1, D).

% Задача 6

qsort([], []).
qsort([H|T], Lst):-
  divide(T, H, GLst, SLst),
  qsort(GLst, GLst1),
  qsort(SLst, SLst1),!,
  append(SLst1, [H|GLst1], Lst).
divide([], _, [], []):-!.
divide([H|T], N, [H|GLst], SLst):-
  H >= N, !, divide(T, N, GLst, SLst).
divide([H|T], N, GLst, [H|SLst]):-
  divide(T, N, GLst, SLst).

% Задача 7

p(Lst, X) :-
    helper7(Lst, [], [], X).
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.
helper7(Lst, SubLstCur, SubLstMax, Res) :-
    ( 
        Lst = [] -> (length(SubLstCur, Cur), length(SubLstMax, Max), Cur > Max -> reverse(SubLstCur, Res) ; reverse(SubLstMax, Res))
        ;
        Lst = [H|T], 
        (
            (SubLstCur = []; SubLstCur = [S|_], H > S) -> helper7(T, [H|SubLstCur], SubLstMax, Res)
            ;
            length(SubLstMax, Max), length(SubLstCur, Cur),
            (
                Cur > Max -> helper7(Lst, [], SubLstCur, Res)
                ;
                helper7(Lst, [], SubLstMax, Res)    
            )
        )
    ).
