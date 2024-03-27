:- module(_,_,[assertions,regtypes]).
% :- module(_,_,[]).           % For pure LP, depth-first search rule
%:- module(_,_,['sr/bfall']).   % For pure LP, breadth-first search rule, all predicates

:- doc(author_data/4,"Defines authors in Deliverit system. It is defined as: @includedef{author_data/4}").
:- prop author_data(Surname1, Surname2, Name, ID) 
#"@var{Surname1} is your first surname.\n
@var{Surname2} is your second surname.\n
@var{Name} is your name.\n
@var{ID} is university identifier.".
author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

:- doc(title, "PROLOG: Practica 1").
:- doc(author, "Eduardo Gil Alba, z170238").
ymd_date(2024/03/26).

:- doc(charge/1,"Define los posibles valores constantes que contendrán las células cargadas. @includedef{charge/1}\n").
charge( +++++++ ).
charge( ++++++ ).
charge( +++++ ).
charge( ++++ ).
charge( +++ ).
charge( ++ ).
charge( + ).
charge( 0 ).

:- doc(my_list/1,"Define una lista según la representación interna @tt{[Head|Tail]} en Prolog, que almacenará los valores de 
las células cargadas.
@subsubsection{Casos base} 
@begin{verbatim}
my_list([+++++++]).
my_list([++++++]).
my_list([+++++]).
my_list([++++]).
my_list([+++]).
my_list([++]).
my_list([+]).
my_list([0]).
my_list([]).
@end{verbatim}
Por último, el predicado @pred{list/1} que, de manera recursiva, comprueba que todos los elementos sean cargas y sea una 
estructura de lista.
@begin{verbatim}
my_list([H|T]) :- 
   charge(H), 
   my_list(T).
@end{verbatim}
").

%my_list([+++++++]).
%my_list([++++++]).
%my_list([+++++]).
%my_list([++++]).
%my_list([+++]).
%my_list([++]).
%my_list([+]).
%my_list([0]).

my_list([H]) :- charge(H).

my_list([H|T]) :- 
   my_list(T).

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES DE LISTA

mylength([],0).
mylength([_|T],s(N)) :-
   mylength(T,N).

get([Elem|_], s(0), Elem).
get([_|Rest], s(Index), Elem) :-
   get(Rest, Index, Elem).

myappend([],L,L) :- 
   list(L).
myappend([X|Xs],Ys,[X|Zs]) :- 
   myappend(Xs,Ys,Zs).

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES ARITMETICAS

igual(0,0).
igual(s(N), s(N)) :-
   igual(N,N).

:- doc(f/1,"Define la equivalencias entre las cargas y números naturales descritos en notación de Peano, para poder realizar 
las operaciones aritméticas. @includedef{f/2}\n ").

f(+++++++, s(s(s(s(s(s(s(0)))))))).
f(++++++, s(s(s(s(s(s(0))))))).
f(+++++, s(s(s(s(s(0)))))).
f(++++, s(s(s(s(0))))).
f(+++, s(s(s(0)))).
f(++, s(s(0))).
f(+, s(0)).
f(0, 0).

:- doc(plus/3,"Define la operación @tt{+} entre dos números naturales descritos en notación de Peano. @includedef{plus/3}\n
   En el caso base, la suma de cualquier número con 0, es el mismo número. La llamada recursiva decrementa el valor del primer 
   operando hasta ser 0, el caso base, que asigna el segundo operando al resultado y al regresar, incrementa el resultado tantas
   veces como llamadas recursivas se hayan realizado.  ").

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

:- doc(minus/3,"Define la operación @tt{-}  entre dos números naturales descritos en notación de Peano, empleando el predicado
de la suma @pred{plus/3}. @includedef{minus/3}\n ").

minus(A, B, C) :- plus(C, B, A).

:- doc(less/2,"Define la operación @tt{>} entre dos números naturales descritos en notación de Peano. @includedef{less/2}\n ").

less(0,s(_X)).
less(s(X),s(Y)) :- less(X,Y).

:- doc(div/3,"Define la operación @tt{/} entre dos números naturales descritos en notación de Peano.  @includedef{div/3}\n 
 Vista la división como sucesivas restas al dividiendo hasta ser 0 o el resto. ").

div(X, Y, s(0)) :- minus(X, Y, Z), less(Z, Y). 
div(X, Y, s(Q)) :- minus(X, Y, Z), div(Z, Y, Q).

%------------------------------------------------------------------------------------------------------------------------%

:- doc(basic_surface/1,"Define una superficie de celulas cargadas @includedef{basic_surface/1}\n
@begin{note}
@bf{Note:} @pred{basic_surface/1} must have at least one sublist with a charged cell.
@end{note}").
:- prop basic_surface(CellList) 
#"@var{CellList} is a list with charged cells.".


:- test basic_surface(X)   
   : (X = [[]])  
   => fails
   # "Cannot be a list with a empty sublist".


basic_surface([[H|T]]) :-  
   charge(H), 
   my_list(T).

basic_surface([_L|S2]) :-  
   basic_surface(S2).





:- doc(surface/1,"Defines a surface of charged cells, represented by a list of lists with same number of cells. 
It is defined as: @includedef{surface/1}\n").
:- prop surface(CellList) 
#"@var{CellList} is a list with charged cells.".

surface([L|L2]) :- 
   basic_surface([L|L2]), 
   mylength(L, Tam), 
   surface_acc([L|L2], Tam).

surface_acc([L],Tam) :- mylength(L, Tam).
surface_acc([[H|T]|T2], Acc) :- 
   mylength([H|T],NewAcc), 
   igual(Acc,NewAcc), 
   surface_acc(T2,NewAcc).



h_line([L], s(0), L).
h_line(S, N, C) :-
   get(S, N, C),
   basic_surface(S).
   
h_line(S, N, C) :-
   get(S, N, C),
   surface(S).

v_line([L|L2], N, C) :-
   surface([L|L2]),
   v_line_aux([L|L2], N, C).
   


v_line_aux([L|L2], N, [E|T]) :-
   get(L, N, E),
   v_line_aux(L2, N, T).






h_sum([], 0).
h_sum([H|T], Suma) :-
   h_sum(T, SumaResto),
   f(H, N),
   plus(N, SumaResto, Suma).

total_charge([],0).
total_charge([L|S2], T) :-
   total_charge(S2, SumaResto),
   h_sum(L, SumaFila),
   plus(SumaFila, SumaResto, T).

total_cells([L], N) :- mylength(L,N).
total_cells([L|S2], Total) :-
   total_cells(S2, TotalResto),
   mylength(L, Esta),
   plus(Esta, TotalResto, Total).


average(S, A) :-
   total_charge(S, M),
   total_cells(S, T), 
   div(M, T, A).


