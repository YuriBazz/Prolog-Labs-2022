% Задача 1

% [criminal,dauther1,dauther2,father,police,son1,son2,mother]

son(son1).
son(son2).

daughter(daughter1).
daughter(daughter2).

adult(father).
adult(mother).
adult(police).

notsafe_(criminal, X) :- X \= police.
notsafe_(mother, Y) :- son(Y).
notsafe_(father, Y) :- daughter(Y).

notsafe(X, Y) :- notsafe_(X, Y); notsafe_(Y, X).

safe(X, Y) :- \+ notsafe(X, Y).

safebridge([X, Y]) :- (adult(X); adult(Y)), safe(X, Y), !.
safebridge([X]) :- adult(X).

all([son1, son2, father, daughter1, daughter2, mother, criminal, police]).

allsafe(L) :-
    forall(member(H, L),
          ( 
            adult(H)
            ; 
            son(H),
          ( 
            member(mother, L) -> member(father, L) ; true
          )
          ; 
        daughter(H),
          ( 
            member(father, L) -> member(mother, L) ; true
          )
          ; 
          H = criminal, member(police, L)
          )), !.
allsafe([_]).
allsafe([]).

allPairs([H | T], [H, P2]) :-
    member(P2, T).

allPairs([_ | T], P) :-
    allPairs(T, P).
    
step_(state(Left1, left), state(Left2, right)) :-
    ( 
        allPairs(Left1, OnBridge)
        ; 
        member(A, Left1), OnBridge = [A]
    ),
    safebridge(OnBridge),
    subtract(Left1, OnBridge, Left2), allsafe(Left2),
    all(All), subtract(All, Left2, Right), allsafe(Right).

step(state(Left1, left), state(Left2, right)) :-
    step_(state(Left1, left), state(Left2, right)).

step(state(Left1, right), state(Left2, left)) :-
    all(All), subtract(All, Left1, Right1),
    step_(state(Right1, left), state(Right2, right)), subtract(All, Right2, Left2).

notequal(state(L1, P1), state(L2, P2)) :-
    \+ (
            P1 = P2,
            sort(L1, L),
            sort(L2, L)
       ).
mainFunction(Inp, Outp, PrevSteps, [Step | Steps]) :-
    Step = step(Inp, S1), Step, 
    forall(member(step(State1, _), PrevSteps), notequal(State1, S1)), 
    ( 
        S1 = Outp -> Steps = [] ; mainFunction(S1, Outp, [Step | PrevSteps], Steps)
    ).

japanFerry :-
    all(All),
    findall(Steps, mainFunction(state(All, left), state([], _), [], Steps), Solutions),
    member(Solution, Solutions),(forall(member(Step, Solution),printStep(Step))).

printList([]).
printList([H|L]) :- write(H), write(' '),  printList(L).

printStep(step(state(L1, P1), state(L2, _))) :-
    (
        P1 = left -> subtract(L1, L2, M1), printList(M1), write(:), write(' '), write(moved), write(' '), write(to), write(' '),  write(right), write(' '), write(shore), nl
        ;
        P1 = right -> subtract(L2, L1, M2), printList(M2), write(:), write(' '), write(moved), write(' '), write(to), write(' '),  write(left), write(' '), write(shore), nl
    ).


    
    