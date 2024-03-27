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

:- doc(charge/1,"Defines possible cell charge values. @includedef{charge/1}\n").
charge( +++++++ ).
charge( ++++++ ).
charge( +++++ ).
charge( ++++ ).
charge( +++ ).
charge( ++ ).
charge( + ).
charge( 0 ).

:- doc(my_list/1,"Defines a list according to the internal representation @tt{[Head|Tail]} in Prolog, which will stores
cell charges values. @includedef{my_list/1}\n
The predicate @pred{list/1} is called recursively, checking that all elements are charges and that it is a list structure. ").

%my_list([+++++++]).
%my_list([++++++]).
%my_list([+++++]).
%my_list([++++]).
%my_list([+++]).
%my_list([++]).
%my_list([+]).
%my_list([0]).

my_list([H]) :- charge(H).

my_list([_H|T]) :- 
   my_list(T).

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES DE LISTA

mylength([],0).
mylength([_|T],s(N)) :-
   mylength(T,N).

get([Elem|_], s(0), Elem).
get([_|Rest], s(Index), Elem) :-
   get(Rest, Index, Elem).


%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES ARITMETICAS
:- doc(f/1,"Defines equivalences between charges values and natural numbers in Peano notation, in order to be able to perform 
arithmetic operations. @includedef{f/2}\n ").

f(+++++++, s(s(s(s(s(s(s(0)))))))).
f(++++++, s(s(s(s(s(s(0))))))).
f(+++++, s(s(s(s(s(0)))))).
f(++++, s(s(s(s(0))))).
f(+++, s(s(s(0)))).
f(++, s(s(0))).
f(+, s(0)).
f(0, 0).

:- doc(equal/2,"Defines equality operator @op{==} between two natural numbers in Peano notation. @includedef{equal/2}\n").

:- prop equal(N1, N2)
#"@var{N1} is a natural number.\n
@var{N2} is a natural number.\n
@pred{equal/2} will be true if both numbers are equal".

equal(0,0).
equal(s(N), s(N)) :-
   equal(N,N).


:- doc(plus/3,"Defines sum operator @op{+} between two natural numbers in Peano notation. @includedef{plus/3}\n
   In case base, the sum of any number with 0, is the same number. The recursive call decrements the value of the first 
   operand to 0, reaching case base, which assigns the second operand to the result and, when returning, increments the 
   result as many times as recursive calls have been made.  ").

:- prop plus(Op1, Op2, Resultado)
#"@var{Op1} is a natural number.\n
@var{Op2} is a natural number.\n 
@var{Resultado} is the result.".

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).


:- doc(minus/3,"Defines subtraction operator @op{-} between two natural numbers in Peano notation, using @pred{plus/3}. @includedef{minus/3}\n ").

minus(X, Y, Z) :- plus(Z, Y, X).


:- doc(less/2,"Define la operación @op{>} entre dos números naturales descritos en notación de Peano. @includedef{less/2}\n ").

less(0,s(_X)).
less(s(X),s(Y)) :- less(X,Y).

:- doc(div/3,"Define la operación @op{/} entre dos números naturales descritos en notación de Peano.  @includedef{div/3}\n 
 Division is viewed as successive subtractions from the dividend until it becomes 0 or the remainder. ").

div(X, Y, s(0)) :- minus(X, Y, Z), less(Z, Y). 
div(X, Y, s(Q)) :- minus(X, Y, Z), div(Z, Y, Q).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% DEFINICIONES DE LAS SUPERFICIES

:- doc(basic_surface/1,"Defines a surface of cell charges represented by a list of lists, a cell charges matrix. Internal
representation will be @tt{[List | [Lists]]}. @p
@begin{note}
@bf{Note:} @pred{basic_surface/1} must contains at least one sublist with a charged cell.
@end{note}
@p It is defined as: @includedef{basic_surface/1}\n").

:- prop basic_surface(OneCellMatrix) 
#"@var{OneCellMatrix} is a one row matrix with charged cells.".

:- prop basic_surface(CellMatrix) 
#"@var{CellMatrix} is a matrix with charged cells.".


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
   equal(Acc,NewAcc), 
   surface_acc(T2,NewAcc).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES CON SUPERFICIES

h_line([L], s(0), L).
   
h_line(S, N, C) :-
   get(S, N, C),
   surface(S).


v_line([], _, []).
v_line([Fila|Filas], Indice, [Elemento|Columna]) :-
   get(Fila, Indice, Elemento),
   v_line(Filas, Indice, Columna).




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

%------------------------------------------------------------------------------------------------------------------------%

% FIN CODIGO
