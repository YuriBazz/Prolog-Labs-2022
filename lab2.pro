% Задание 1

toSquares(_, 0, 0).
toSquares(A, B, C) :-
    A =< B,
    A1 is A mod B, B1 is B - A1, toSquares(B1, A1, C1), C is C1 + 1
    ;
    A > B,
    toSquares(B, A, C).

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
        N1 is N div 10, helper(N1, K)
    ).
qntDigit(A, B, K, N) :-
    A > B, !, N = 0
    ;
    A1 is A + 1,
    (
        kInNumber(A, K), !, qntDigit(A1, B, K, N1), N is N1 + 1
        ;
        qntDigit(A1, B, K, N)
    ).

%Задание 3

sumCubes(A, B) :-
    var(A), step_for_vars(A, B, 0)
    ;
    number(A), step_for_numbers(A, B, 0, 0).
step_for_vars(A, B, C) :- 
    A is C, step_for_numbers(A, B, 0, 0)
    ;
    C1 is C + 1, step_for_vars(A, B, C1).

step_for_numbers(N, R, A, B) :-
    R = B, N is A, !
    ;
    N > A, A1 is A + 1, A2 is A1 ^ 3, B1 is B + A2, step_for_numbers(N, R, A1, B1).

%Задание 4

insert(B, N, T) :-
    B = tr(A,nil,nil),
    (
    N > A, T = tr(A,nil,tr(N,nil,nil))
    ;
    N < A, T = tr(A,tr(N,nil,nil),nil)
    ;
    N =:= A, T = tr(A,nil,nil)
    ).
insert(A, N, T) :-
    A = tr(B, C, D),
    (
        B =:= N, T = A
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
    \+ contains(T, N), X = T.
remove(nil, _, nil).
remove(T, N, X) :-
    T = tr(H, L, R),
    (
        N < H, !, remove(L, N, X1), X = tr(H, X1, R)
        ;
        N > H, !, remove(R, N, X2), X = tr(H, L, X2)
        ;
        N =:= H, L \== nil, R \== nil, !, minTree(R, N1), remove(R, N1, R1), X = tr(N1, L, R1)
        ;
        N =:= H, R = nil, L \== nil, X = L
        ;
        N =:= H, L = nil, R \== nil, X = R
        ;
        N =:= H, L = nil, R = nil, X = nil
    ).
minTree(T, N) :-
    T = tr(H, L, _),
    (
        L = nil, N = H 
        ;
        minTree(L, N1), N = N1
    ).