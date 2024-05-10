:- module(_,_,[classic ,assertions ,regtypes]).

:- prop camino_arbol_arbol(T1,T2, D) :: (arbol(T1), arbol(T2), number(D)).

:- dynamic(camino_arbol_arbol/3). % Distancias entre arboles
camino_arbol_arbol( arbol1 , arbol2 , 18 ).
camino_arbol_arbol( arbol2 , arbol3 , 12 ).
camino_arbol_arbol( arbol1 , arbol3 , 19 ).
camino_arbol_arbol( arbol4 , arbol3 , 8 ).
camino_arbol_arbol( arbol4 , arbol2 , 20 ).
:- dynamic(camino_arbol_pozo/2).
% Distancias entre el pozo y los arboles.
:- prop camino_arbol_pozo(T, D) :: (arbol(T), number(D)).

camino_arbol_pozo( arbol1 , 13 ).
camino_arbol_pozo( arbol2 , 19 ).
camino_arbol_pozo( arbol3 , 22 ).
camino_arbol_pozo( arbol4 , 34 ).
:- dynamic(necesita/2).
% Cantidad de agua necesaria para regar cada arbol.
:- pred necesita(T, D) 
    :: (arbol(T), number(D))
    #"@includedef{necesita/2}".

necesita( arbol1 , 2 ).
necesita( arbol2 , 1 ).
necesita( arbol3 , 2 ).
necesita( arbol4 , 4 ).
:- dynamic(capacidad/1).
% Capacidad del cubo.
:- prop capacidad(C) :: (number(C)).
capacidad(5).

%----------------------------------------------------------------------------------%
:- doc(author_data/4,"Defines authors in Deliverit system.").

author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

:- doc(title, "ISO Prolog Programming: Practise 2").
:- doc(author, "Eduardo Gil Alba").
:- doc(module, "
@section{Introduction}
On the state, daily watering of trees is essential. To automate this task, the state owner has
procured a robotic device capable of autonomously watering trees and preparing soil for new
plantings. This robotic system initiates operation at dawn, tasked with watering each tree
individually. Each tree is identified by a constant Prolog term and requires a specific 
amount of water, denoted by a binary predicate, indicating if tree T needs C units of water
daily. @p
@begin{verbatim}
💧🌳                      🌳💧
 2 1 ________18____________ 2 1
   |\\                    / |
   | \\                  /  |
   |  13               19  |
   |   \\____      ____/    |
  19     ___ ⛲🤖____     15
   |    /             \\    |
   |  22               \\   |
   |  /                 34 |
   | /                   \\ |
💧🌳                      🌳💧
 2 3 _________8___________ 4 4
@end{verbatim}
The watering procedure is as follows: every morning, the robot positions itself by the well.
Upon activation, it fills its initially empty bucket with water from the well, then proceeds 
to water each tree sequentially. If the bucket empties before all trees are watered, the 
robot returns to the well, refills the bucket, and resumes watering. Once all trees are 
watered or the bucket empties again, the robot refills the bucket if necessary and continues 
the task. Upon completion, it notifies the state owner telematically.@p
Additionally, the robot must adhere to the following rules: @p
@begin{enumerate}
@item It must never attempt to water a tree without sufficient water in the bucket, constrained 
by the bucket's capacity (determined by the predicate).
@item Paths connecting trees and the well vary in length, each taking a specific time to 
traverse. These paths are represented by ternary and binary predicates, ensuring efficient 
movement.
@item Due to potential obstacles (e.g., ponds, buildings, fences), not all points on the 
state may be connected. Consequently, the robot may encounter inaccessible areas.
@item The robot can return to the well only when the bucket is empty, except after 
completing all watering cycles.
@item Bucket refilling at the well always takes a fixed amount of time.
@item Each tree must be visited and watered exactly once during the task execution.
@item All distances, time units, and water quantities involved are represented as integers.
@end{enumerate}
Moreover, the robot, simulating artificial intelligence, anticipates future tasks such as 
digging holes for new tree planting, prompting it to prolong watering tasks while adhering 
to the aforementioned rules. @p
It's essential to note that state descriptions are solely based on predicate representations, 
ensuring consistency and compatibility with the implemented predicates.").

%---------------------------------------------------------------------------------%
% OPERACIONES CON LISTAS
is_set([]).
is_set([H|T]) :-
    \+ member(H, T),  
    is_set(T). 

permute([], []).
permute(LT, [H|P]) :-
    select(H, LT, T),
    permute(T, P).
%---------------------------------------------------------------------------------%

:- prop arbol(T)
    #" A property, defined as follows:
    @includedef{arbol/1}
    @var{T} is a tree.".

arbol(arbol1).
arbol(arbol2).
arbol(arbol3).
arbol(arbol4).

:- prop lista_de_arboles(LT) 
    #"@var{LT} is a list of trees on the state".

lista_de_arboles(LT) :-
    setof(X,arbol(X),LT).

:- pred de_pozo_a_regar_arbol(A,DA,NV,ND) 
    :: (arbol(A), number(DA), number(NV), number(ND))
    #"@includedef{de_pozo_a_regar_arbol/4}".

/* Asumo que si el robot esta en el pozo, el cubo esta lleno de agua, se comprueba la cantidad
que necesita el arbol, pero en principio todos los arboles deberian de necesitar menos de 
la capacidad o nunca seran regados */

de_pozo_a_regar_arbol(A,DA,NV,ND) :-
    arbol(A), 
    capacidad(C),
    necesita(A, V),
    NV is C - V,
    camino_arbol_pozo(A, D),
    ND is DA + D.

:- test (de_pozo_a_regar_arbol(A, DA, NV, ND)) 
    : (A = arbol1, DA = 0)
    => (NV = 8, ND = 13)
    #"Correctly use".

:- test (de_pozo_a_regar_arbol(A, DA, NV, ND))
    : (DA = 0, ND = 22)
    => (A = arbol3, NV = 8)
    #"Asking which tree with time passed given".

:- test (de_pozo_a_regar_arbol(A, DA, NV, _))
    : (DA = 0, NV = 8)
    => (A = arbol1 ; A = arbol3, ND = 13 ; ND = 22)
    #"Asking which tree with new volume water given".

:- test (de_pozo_a_regar_arbol(A, _, _, _))
    : (A = arbol5) + fails
    #"Non existing tree on the state given".



:- pred regar_otro_arbol(A,NA,V,NV,D,ND) 
    :: (arbol(A),arbol(NA),number(V),number(NV),number(D),number(ND))
    #"@includedef{regar_otro_arbol/6}".

regar_otro_arbol(A,NA,V,NV,D,ND) :-
    arbol(A),
    arbol(NA),
    necesita(NA, N),
    V >= N,
    NV is V - N,
    camino_arbol_arbol(A, NA, T), 
    !,
    ND is D + T.

regar_otro_arbol(A,NA,V,NV,D,ND) :-
    arbol(A),
    arbol(NA),
    necesita(NA, N),
    V >= N,
    NV is V - N,
    camino_arbol_arbol(NA, A, T), 
    ND is D + T.

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol1, NA = arbol2, V = 4, D = 20)
    => (ND = 38, NV = 3) + not_fails
    #"Correctly use".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol2, NA = arbol1, V = 4, D = 20)
    => (ND = 38, NV = 2) + not_fails
    #"Both directions with missing path clause".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol5) + fails
    #"Non existing tree on the state".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (V = 13) + fails
    #"Volume is over the capacity".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol2, NA = arbol1, V = 1, D = 20) + fails
    #"Not enough water to target tree".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1], DA = 20) + not_fails
    #"Test 5.1: De un árbol a otro".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol1,arbol4], DA = 20) + fails
    #"Test 5.4: De un árbol a otro, movimiento no posible".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1,arbol3], DA = 20) + not_fails
    #"Test 5.5: De un árbol a otros dos".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1,arbol3,arbol4], DA = 20) + not_fails
    #"Test 5.6: De un árbol a los otros tres".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1,arbol4,arbol3], DA = 20) + fails
    #"Test 5.7: Movimiento no posible".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol4,arbol3,arbol1], DA = 20) + not_fails
    #"Test 5.8: De un árbol a los otros tres".

% [3,1,2,4] 0
% P   ->  3   ->  1   ->  2   ->  P   ->  4    ->  P
% (0) -> (22) -> (41) -> (59) -> (78) -> (112) -> (146)
% (5) -> (3)  -> (1)  -> (0)  -> (5)  -> (1)   -> (5)
% P -> 1 -> 2 -> P
% [1,2,3,4]
%  P -> 1 -> 2 -> 3 -> 4 -> P

:- pred (movimiento_desde_pozo(A, DA, DT)) 
    :: (lista_de_arboles(A), number(DA), number(DT))
    #"@includedef{movimiento_desde_pozo/3}".

movimiento_desde_pozo([H], DA, DT) :-
    de_pozo_a_regar_arbol(H, DA, _, DT).

movimiento_desde_pozo([H|[K|T]], DA, DT) :-
    is_set([H|[K|T]]),
    capacidad(C),
    camino_arbol_arbol(H,K,_), !,
    necesita(H, V),
    NV is C - V,
    movimiento_desde_pozo([H], 0, NT),
    NA is DA + NT,
    movimiento_desde_arbol([K|T], H, NV, NA, DT), !.

movimiento_desde_pozo([H|[K|T]], DA, DT) :-
    is_set([H|[K|T]]),
    capacidad(C),
    camino_arbol_arbol(K,H,_), !,
    necesita(H, V),
    NV is C - V,
    movimiento_desde_pozo([H], 0, NT),
    NA is DA + NT,
    movimiento_desde_arbol([K|T], H, NV, NA, DT), !.

% A = 1 T = [2,4] V = 5 DA = 74
%   1 -> 2 (V = 4 DT = 92) -> 4 (V = 0 DT = 112) -> P DT = 146 
%
%

:- pred (movimiento_desde_arbol(T, A, V, DA, DT)) 
    :: (lista_de_arboles(T), arbol(A), number(V), number(DT))
    #"@includedef{movimiento_desde_arbol/5}".

movimiento_desde_arbol([], A, _V, DA, DT) :-
    movimiento_desde_pozo([A], DA, DT), !.

movimiento_desde_arbol([H|T], A, V, DA, DT) :-
    necesita(H, V2),
    V >= V2, !,
    regar_otro_arbol(A, H, V, NV, DA, ND),
    movimiento_desde_arbol(T, H, NV, ND, DT).

movimiento_desde_arbol([H|T], A, _V, DA, DT) :-
    movimiento_desde_pozo([A], 0, N),
    T1 is DA + N,
    movimiento_desde_pozo([H], T1, NT), !,
    necesita(H, VH),
    capacidad(C),
    NV is C - VH,
    movimiento_desde_arbol(T, H, NV, NT, DT).

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1], A = arbol2, V = 10, DA = 20) + not_fails
    #"Test 6.1: De un árbol a otro".

:-test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol2], A = arbol1, V = 10, DA = 20) + not_fails
    #"Test 6.2: De un árbol a otro (en sentido inverso)".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol3], A = arbol1, V = 10, DA = 20) + not_fails
    #"Test 6.3: De un árbol a otro".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3], A = arbol2, V = 10, DA = 20) + not_fails
    #"Test 6.5: De un árbol a otros dos".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3,arbol4], A = arbol2, V = 10, DA = 20) + not_fails
    #"Test 6.6: De un árbol a los otros tres".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3,arbol4], A = arbol2, V = 8, DA = 20) + not_fails
    #"Test 6.7: De un árbol a los otros tres".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol4,arbol3,arbol1], A = arbol2, V = 0, DA = 20) + not_fails
    #"Test 6.8: De un árbol a los otros tres".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3,arbol4], A = arbol2, V = 7, DA = 20) + fails
    #"Test 6.11: Movimiento no posible".

:- pred (trayectoria_valida(A,D,T)) 
    :: (lista_de_arboles(A), number(D), lista_de_arboles(T))
    #"@includedef{trayectoria_valida/3}".

trayectoria_valida(A, D, T) :-
    permute(A, T), 
    movimiento_desde_pozo(T, 0, D).


max_list( [H], H).
max_list([H,K|T],M) :- H >= K, !, max_list([H|T],M). 
max_list([H,K|T],M) :- H < K,  max_list([K|T],M).

:- pred (riego(T,D)) 
    : (var(T), var(D)) 
    => (lista_de_arboles(T), number(D))
    #"@includedef{riego/2}".

riego(T, D) :-
    lista_de_arboles(LT),
    findall(D1, trayectoria_valida(LT, D1, _), S),
    max_list(S, D),
    nth(N, S, D),
    findall(T1, trayectoria_valida(LT, _, T1), S2),
    nth(N, S2, T).
%------------------------------------------------------------------------------------%
% TESTS DINAMICOS
anadir_camino_pozo(A,D) :- assert(camino_arbol_pozo(A,D)).
eliminar_camino_pozo(A,D) :- retract(camino_arbol_pozo(A,D)).
anadir_camino_arbol(A1,A2,D) :- assert(camino_arbol_arbol(A1,A2,D)).
eliminar_camino_arbol(A1,A2,D) :- retract(camino_arbol_arbol(A1,A2,D)).
anadir_capacidad(C) :- assert(capacidad(C)).
eliminar_capacidad(C) :- retract(capacidad(C)).
%------------------------------------------------------------------------------------%