% Задача 1 (3)

even(X) :-  X mod 2 =:= 0.

% Задача 2 (4)

doubleFact(0, 1).
doubleFact(1, 1).
doubleFact(X, Y) :-
    X > 0, X1 is X-2, doubleFact(X1, Y1), Y is Y1*X.

% Задача 3 (5)

prime(X) :- helper3(X,2), nl.
helper3(N, D) :- (N =:= D) ; (N > D, N mod D =\= 0, D1 is D + 1, helper3(N, D1)).

% Задача 4 (6)

sirakuz(N, A1) :- helper4(N, A1, 1).
helper4(N, CurEl, CurN) :- 
    CurN > N
    ;
    CurN =< N,
    (
        CurEl mod 2 =:= 0, NextEl is CurEl div 2, NextN is CurN + 1, write(CurEl), nl, helper4(N, NextEl, NextN)
        ;
        CurEl mod 2 =\= 0, NextEl is (3 * CurEl + 1), NextN is CurN + 1, write(CurEl), nl, helper4(N, NextEl, NextN)
    ).

% Задача 5 (7)

numOfDays(Month, Number) :-
    Month = feb, Number = 28
    ;
    member(Month, [jan, mar, may, jul, aug, oct, dec]), Number = 31
    ;
    member(Month, [apr, jun, sep, nov]), Number = 30.

nextDate(A1, X) :-
    A1 = date(M1, D1), D1 > 0, numOfDays(M1, Number1),
    (
        var(X),
        (
            D1 =:= Number1, monthDelta(M1, M2), X = date(M2, 1)
            ;
            D1 < Number1, D2 is D1 + 1, M2 = M1, X = date(M2, D2)
        )
        ;
        \+ var(X), A1 = date(M1, D1), X = date(M2,D2), D1 =< Number1, D2 > 0, numOfDays(M2, Number2), D2 =< Number2,
        (
            M1 = M2, D1 =:= D2 - 1
            ;
            monthDelta(M1, M2), numOfDays(M1, D1), D2 =:= 1
        )
    ).

monthDelta(M1, M2) :- 
    M1 = dec, M2 = jan
    ;
    Months = [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec], nth(N1, Months, M1), nth(N2, Months, M2), N1 =:= N2 - 1.