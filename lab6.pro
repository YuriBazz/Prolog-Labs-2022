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

% Задача 2

impl(A, B) :- call(A) -> call(B) ; true.

study(yes).
study(no).

students :-
    (study(Andrew), study(Dmitry), study(Boris), impl((Andrew = yes , Dmitry = yes), (Boris = no))),
    (study(Andrew), study(Dmitry), study(Boris), study(Victor), impl((Andrew = yes , Dmitry = no), (Boris = yes , Victor = no))),
    (study(Andrew), study(Victor), Andrew = Victor),
    (study(Dmitry), study(Gregory), impl((Dmitry = yes), (Gregory = no))),
    (study(Boris), study(Victor), study(Dmitry), (impl((Boris = no, Victor = no), (Dmitry = yes)))),
    (study(Boris), study(Victor), study(Dmitry), study(Gregory), impl((Boris = no, Victor = yes), (Dmitry = no, Gregory = yes))),
    (study(Boris), study(Andrew), impl((Boris = yes), (Andrew = yes))),
    write('Andrew ') , write(Andrew), nl,
    write('Boris ') , write(Boris), nl,
    write('Dmitry ') , write(Dmitry), nl,
    write('Gregory ') , write(Gregory), nl,
    write('Victor ') , write(Victor), nl.

% Задача 3

zip([],_,_,Res) :- !, Res = [].
zip(_,[],_,Res) :- !, Res = [].
zip([H1|T1],[H2|T2],Pred,[HR|TR]) :-
    Term =.. [Pred,H1,H2,HR], call(Term), zip(T1,T2,Pred,TR).

trueAndFalse([Name1-Place1,Name2-Place2],Lst) :-
    memberchk(Name1-Given1,Lst), memberchk(Name2-Given2,Lst),
    (Place1 = Given1, Place2 \= Given2 ; Place1 \= Given1, Place2 = Given2).

all([],_,_).
all([H|T],Pred,Args) :-
Try =.. [Pred,H|Args], call(Try), all(T,Pred,Args).

start :-
    Students = [Misha, Kolya, Sergey],
    permutation([yes, no], X),
    zip(Students, X , '-', Results),
    Y = [ [Misha-no, Kolya-no], [Misha-no, Sergey-yes ], [Sergey-no, Misha-yes] ],
    all(Y,trueAndFalse,[Results]),
    write(Results).

% Задача 4
 % Monday Tuesday Wednesday Thursday Friday Saturday Sunday 

% Задача 5
% все списки задаются видом [[буква клетки, номер клетки], [буква клетки, номер клетки], [буква клетки, номер клетки], ...]
numbers([1,2,3,4,5,6,7,8]).
letters([a,b,c,d,e,f,g,h]).

cells(Lst) :- letters(L1), numbers(L2), append(L1, L2, L), allPairs(L, Lst), Lst = [H,T], member(H, L1), member(T, L2). 

badCells([L, N], Lst) :-
    N1 is N - 1,
    (
        N1 > 0, !,   
            (
                L = h, !,
                (Lst = [g, N1] ; Lst = [h, N])
                ;
                L = a, !,
                (Lst = [b, N1] ; Lst = [a, N])
                ; 
                letters(L1), mySublist([A, L, B], L1), (Lst = [A, N1] ; Lst = [L, N] ; Lst = [B, N1])
            )
        ;
        N1 = 0, !,
            Lst = [L, N]
    ).

goodCells(ListOfPawns, Lst) :- 
    findall(Cells, cells(Cells), AllCells), 
    findall(L, (member(Pawn, ListOfPawns), badCells(Pawn, L)), AllBadCells),
    findall(Cell, (member(Cell, AllCells), \+ member(Cell, AllBadCells)), Lst).

isCell([L, N]) :-  numbers(L1), letters(L2), member(N, L1), member(L, L2).

kingStep([L, N], Cell2) :- 
    N1 is N + 1, N2 is N1 + 1, N3 is N - 1, (badCells([L, N], Cell2); badCells([L,N1], Cell2) ; badCells([L, N2], Cell2); Cell2 = [L, N3]), 
    Cell2 \= [L, N], Cell2 \= [L, N2], isCell(Cell2).


distance([L1, N1], [L2, N2], N) :-
    letters(List), nth(H1, List, L1), nth(H2, List, L2),
    N3 is (((N1 - N2) ^ 2) + ((H1 - H2) ^ 2)) ^ (1/2),
    N = N3.

allKingSteps(Cell, Good, List) :- findall(Step, (member(Step, Good), kingStep(Cell, Step)), List).

findMinDistance(List, X, Res) :-
    List = [H|T], distance(H, X, D), helperDistance(T, X, D, H, Res).

helperDistance(List, X, D, Cur, Res) :-
    List = [] -> Res = Cur ; (List = [H|T], distance(H, X, N), (N < D, !, helperDistance(T, X, N, H, Res); N > D, !, helperDistance(T, X, D, Cur, Res) ; N = D, !, 
    (helperDistance(T, X, D, Cur, Res) ; helperDistance(T, X, N, H, Res)))).


bestStep(Cell, List, X, Cell1) :-
    allKingSteps(Cell, List, List1), findMinDistance(List1, X, Cell1).

king_path(ListOfPawns, Path) :-
    goodCells(ListOfPawns, List), lastHelper(List, [a,8], [h,1], [], Path).

lastHelper(List, Start, Goal, Path, Res) :-
    Start = Goal -> Res = [Goal|Path]
    ;
    bestStep(Start, List, Goal, Next), lastHelper(List, Next, Goal, [Start|Path], Res).

king_path1(ListOfPawns, N) :- findall(Path, king_path(ListOfPawns, Path), Solution), Solution = [H|_], length(H, N).
king_path2(ListOfPawns, Path) :- king_path(ListOfPawns, Path1), reverse(Path1, Path).

    
    
