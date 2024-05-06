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
capacidad(11).

%----------------------------------------------------------------------------------%
:- doc(author_data/4,"Defines authors in Deliverit system.").

author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

:- doc(title, "ISO Prolog: Practise 2").
:- doc(author, "Eduardo Gil Alba").
:- doc(module, "").

:- prop arbol(T)
    #"@var{T} is a tree.".

arbol(arbol1).
arbol(arbol2).
arbol(arbol3).
arbol(arbol4).

:- pred lista_de_arboles(LT) 
    #"@var{LT} is a list of trees on the state".

lista_de_arboles([arbol1, arbol2, arbol3, arbol4]).

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