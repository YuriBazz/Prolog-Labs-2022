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

prime(1).
prime(X) :- helper3(X,2).
helper3(N, D) :- (N =:= D) ; (N > D, N mod D =\= 0, D1 is D + 1, helper3(N, D1)).
deleteNonPrime([], []).
deleteNonPrime([H|T], X) :-
    prime(H), !, deleteNonPrime(T, X1), X = [H|X1]
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
    myPrefix(L1, L2)
    ;
    L2 = [_|T2], mySublist(L1, T2).

% Задача 4

myConfluens(L1, L2, X) :-
    myReverse(L1, Y1), myReverse(L2, Y2), helper4(Y1, Y2, [], X).
helper4(L1, L2, X1, X) :-
    L1 = [], L2 = [], !, X = X1
    ;
    L1 = [], L2 = [H2|T2], !, helper4(L1, T2, [H2|X1], X)
    ;
    L1 = [H1|T1], L2 = [], !, helper4(T1, L2, [H1|X1], X)
    ;
    L1 = [H1|T1], L2 = [H2|T2], !, (H2 > H1 -> helper4(L1, T2, [H2|X1], X); helper4(T1, L2, [H1|X1], X)).

% Задача 5

numlst(N, Lst) :-
    number(N), var(Lst),
    (   
        N = 0, Lst = [0], !
        ;
        helper5(N, Lst, [])
    ).
numlst(N, Lst) :-
    var(N), var(Lst),
    numlst1(N, Lst, 0).
numlst1(N, Lst, X) :-
    N = X, numlst(N, Lst)
    ;
    X1 is X + 1, numlst1(N, Lst, X1).
helper5(N, Lst, X) :-
    N = 0, !, Lst = X
    ;
    N1 is N mod 10, N2 is N div 10, X1 = [N1|X], helper5(N2, Lst, X1).

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

% Задача 7 НЕ РАБОТАЕТ!!!!!!

p(Lst, X) :-
    myReverse(Lst, Y),
    helper7(Y, [], [], X).
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.
helper7(Lst, LstCur, LstMax, X) :-
    Lst = [], !, len(LstCur, N1), len(LstMax, N2),
    (
        N1 > N2, !, X = LstCur
        ;
        X = LstMax
    )
    ;
    Lst = [H|T],
    (
        LstCur = [], !, helper7(T, [H], LstMax, X)
        ;
        LstCur = [H1|_], !,
        (
            H > H1, append(LstCur, [H], Y), helper7(T, Y, LstMax, X)
            ;
            len(LstCur, N1), len(LstMax, N2),
            (
                N1 > N2, !, helper7(T, [], LstCur, X)
                ;
                helper7(T, [], LstMax, X)
            )
        )
    ).
