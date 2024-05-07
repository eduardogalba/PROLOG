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
:- pred necesita(T, D) :: (arbol(T), number(D)).

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

:- doc(title, "ISO Prolog: Practise 2").
:- doc(author, "Eduardo Gil Alba").
:- doc(module, "").

%---------------------------------------------------------------------------------%

:- prop arbol(T)
    #"@var{T} is a tree.".

arbol(arbol1).
arbol(arbol2).
arbol(arbol3).
arbol(arbol4).

:- pred lista_de_arboles(LT) 
    #"@var{LT} is a list of trees on the state".

lista_de_arboles(LT) :-
    setof(X,arbol(X),LT).

:- pred de_pozo_a_regar_arbol(A,DA,NV,ND) :: (arbol(A), number(DA), number(NV), number(ND)).

de_pozo_a_regar_arbol(A,DA,NV,ND) :-
    arbol(A), 
    capacidad(C),
    necesita(A, V),
    NV is C - V,
    camino_arbol_pozo(A, D),
    ND is DA + D.

:- pred regar_otro_arbol(A,NA,V,NV,D,ND) :: (arbol(A),arbol(NA),number(V),number(NV),number(D),number(ND)).

regar_otro_arbol(A,NA,V,NV,D,ND) :-
    arbol(A),
    arbol(NA),
    capacidad(C),
    V =< C,
    necesita(NA, N),
    V >= N,
    NV is V - N,
    camino_arbol_arbol(A, NA, T), 
    !,
    ND is D + T.

regar_otro_arbol(A,NA,V,NV,D,ND) :-
    arbol(A),
    arbol(NA),
    capacidad(C),
    V =< C,
    necesita(NA, N),
    V >= N,
    NV is V - N,
    camino_arbol_arbol(NA, A, T), 
    !,
    ND is D + T.

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol1, NA = arbol2, V = 10, D = 20)
    => (ND = 38, NV = 9)
    #"Correctly use".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol2, NA = arbol1, V = 10, D = 20)
    => (ND = 38, NV = 8)
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

% [3,1,2,4] 0
% P -> 3 -> 1 -> 2 -> P -> 4 -> P
% (0) -> (22) -> (41) -> (59) -> (78) -> (112) -> (146)
% (5) -> (3) -> (1) -> (0) -> (5) -> (1) -> (5)
% P -> 1 -> 2 -> P
% [1,2,3,4]
%  P -> 1 -> 2 -> 3 -> 4 -> P

movimiento_desde_pozo([H], DA, DT) :-
    de_pozo_a_regar_arbol(H, DA, _, DT).

movimiento_desde_pozo([H|T], DA, DT) :-
    capacidad(C),
    necesita(H, V),
    NV is C - V,
    movimiento_desde_pozo([H], 0, NT),
    NA is DA + NT,
    movimiento_desde_arbol(T, H, NV, NA, DT), !.

% A = 1 T = [2,4] V = 5 DA = 74
%   1 -> 2 (V = 4 DT = 92) -> 4 (V = 0 DT = 112) -> P DT = 146 
%
%

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
    movimiento_desde_pozo([H], T1, NT),
    necesita(H, VH),
    capacidad(C),
    NV is C - VH,
    movimiento_desde_arbol(T, H, NV, NT, DT).

%------------------------------------------------------------------------------------%
% TESTS DINAMICOS
anadir_camino_pozo(A,D) :- assert(camino_arbol_pozo(A,D)).
eliminar_camino_pozo(A,D) :- retract(camino_arbol_pozo(A,D)).
anadir_camino_arbol(A1,A2,D) :- assert(camino_arbol_arbol(A1,A2,D)).
eliminar_camino_arbol(A1,A2,D) :- retract(camino_arbol_arbol(A1,A2,D)).
anadir_capacidad(C) :- assert(capacidad(C)).
eliminar_capacidad(C) :- retract(capacidad(C)).
%------------------------------------------------------------------------------------%