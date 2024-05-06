:- module(_,_,[classic ,assertions ,regtypes]).
:- dynamic(camino_arbol_arbol/3). % Distancias entre arboles
camino_arbol_arbol( arbol1 , arbol2 , 18 ).
camino_arbol_arbol( arbol2 , arbol3 , 12 ).
camino_arbol_arbol( arbol1 , arbol3 , 19 ).
camino_arbol_arbol( arbol4 , arbol3 , 8 ).
camino_arbol_arbol( arbol4 , arbol2 , 20 ).
:- dynamic(camino_arbol_pozo/2).
% Distancias entre el pozo y los arboles.
camino_arbol_pozo( arbol1 , 13 ).
camino_arbol_pozo( arbol2 , 19 ).
camino_arbol_pozo( arbol3 , 22 ).
camino_arbol_pozo( arbol4 , 34 ).
:- dynamic(necesita/2).
% Cantidad de agua necesaria para regar cada arbol.
necesita( arbol1 , 2 ).
necesita( arbol2 , 1 ).
necesita( arbol3 , 2 ).
necesita( arbol4 , 4 ).
:- dynamic(capacidad/1).
% Capacidad del cubo.
capacidad(5).