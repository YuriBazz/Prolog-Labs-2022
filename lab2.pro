
% Задание 1

toSquares(_, 0, 0).
toSquares(A, B, C) :-
    B > 0, 
    A1 is A mod B, B1 is B, toSquares(B1, A1, C1), C2 is A div B, C is C1 + C2.

%Задание 2

kInNumber(N, K) :-
    N =:= 0, K =:= 0, !
    ;
    helper2(N, K).
helper2(N, K) :- 
    N > 0 , N2 is N mod 10,
    (
        K =:= N2, !
        ; 
        N1 is N div 10, helper2(N1, K)
    ).
qntDigit(A, B, K, N) :-
    A > B, !, N = 0
    ;
    A1 is A + 1,
    (
        A2 is abs(A), kInNumber(A2, K), !, qntDigit(A1, B, K, N1), N is N1 + 1
        ;
        qntDigit(A1, B, K, N)
    ).

%Задание 3

sumCubes(A, B) :-
    number(B), !, step_for_numbers(X, B, 0, 0), A = X
    ;
    step_for_vars(A, B, 0).
step_for_vars(A, B, C) :- 
   step_for_numbers(X, C, 0, 0), B = C, A = X
   ;
   C1 is C + 1, step_for_vars(A, B, C1).

step_for_numbers(A, B, C, D) :-
    B > D, C1 is C + 1, D1 is D + C1^3, step_for_numbers(A, B, C1, D1)
    ;
    B =:= D, A is C.

%Задание 4
insert(nil, N, T) :-
    T = tr(N, nil,nil), !.
insert(B, N, T) :-
    B = tr(A,nil,nil),
    (
    N > A, !, T = tr(A,nil,tr(N,nil,nil))
    ;
    N < A, !, T = tr(A,tr(N,nil,nil),nil)
    ;
    N =:= A, !, T = tr(A,nil,nil)
    ).
insert(A, N, T) :-
    A = tr(B, C, D),
    (
        B =:= N, !, T = A
        ;
        B < N, insert(D, N, T1), T = tr(B, C, T1)
        ;
        B > N, insert(C, N, T1), T = tr(B, T1, D)
    ).

contains(T, N) :-
    number(N), T = tr(A, B, C),
    (
        N =:= A, !
        ;
        N > A, contains(C, N)
        ;
        N < A, contains(B, N)
    ).
contains(T, N) :-
    var(N), var_contains(T, N).
var_contains(T, N) :-
    T = tr(H, L, R), 
    (
        N = H 
        ;
        var_contains(L, N1), N is N1
        ; 
        var_contains(R, N2), N is N2 
    ).

isSearchTree(T) :- check(T, 1.0, inff).
check(nil, _, _).
check(T, Min, Max) :- 
    T = tr(H, L, R), H @> Min, H @< Max, check(L, Min, H), check(R, H, Max).


remove(T, N, X) :-
    T = nil, !, X = nil
    ;
    T = tr(H, L, R),
    (
        N < H, !, remove(L, N, X1), X = tr(H, X1, R)
        ;
        N > H, !, remove(R, N, X2), X = tr(H, L, X2)
        ;
        L \== nil, R \== nil, minTree(R, N1), remove(R, N1, R1), X = tr(N1, L, R1)
        ;
        R = nil, L \== nil, X = L
        ;
        L = nil, R \== nil, X = R
        ;
        L = nil, R = nil, X = nil
    ).
minTree(T, N) :-
    T = tr(H, L, _),
    (
        L = nil, N = H 
        ;
        minTree(L, N1), N = N1
    ).
