:- module(_,_,[pure,assertions,regtypes]).
%:- module(_,_,[assertions,regtypes]).
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

:- prop charge(C)
   #"@var{C} is cell charge value".

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
%my_list([]).
% OJO: SI descomentas esta parte, creas mas casos y posible bucle infinito con la misma solucion

:- prop my_list(L)
   #"@var{L} is a list with charge cell values.".

my_list([H]) :- charge(H).

my_list([_H|T]) :- 
   my_list(T).

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES DE LISTA

:- doc(mylength/2, "Calculates the size of a list. @includedef{mylength/2}\n").

:- pred mylength(L, N) :: (my_list(L), nat_num(N)).

mylength([],0).
mylength([_|T],s(N)) :-
   mylength(T,N).

:- doc(get/2, "Extracts a specific element from a list. @includedef{get/2}\n").

:- pred get(L, I, E) :: (my_list(L), nat_num(I), charge(E)).

get([Elem|_], s(0), Elem) :- charge(Elem).
get([_|Rest], s(Index), Elem) :-
   get(Rest, Index, Elem).

:- doc(myappend/3, "").

:- pred myappend(L1, L2, R) :: (my_list(L1), my_list(L2), my_list(R)).

myappend([],L,L).
myappend([X|Xs],Ys,[X|Zs]) :- 
    myappend(Xs,Ys,Zs).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES ARITMETICAS
:- doc(f/1,"Defines equivalences between charges values and natural numbers in Peano notation, in order to be able to perform 
arithmetic operations. @includedef{f/2}\n ").

:- pred f(C, N) :: (charge(C), nat_num(N)).

:- pred f(N1, N2) :: (nat_num(N1), nat_num(N2)).

f(+++++++, s(s(s(s(s(s(s(0)))))))).
f(++++++, s(s(s(s(s(s(0))))))).
f(+++++, s(s(s(s(s(0)))))).
f(++++, s(s(s(s(0))))).
f(+++, s(s(s(0)))).
f(++, s(s(0))).
f(+, s(0)).
f(0, 0).

f(s(s(s(s(s(s(s(0))))))), s(s(s(s(s(s(s(0)))))))).
f(s(s(s(s(s(s(0)))))), s(s(s(s(s(s(0))))))).
f(s(s(s(s(s(0))))), s(s(s(s(s(0)))))).
f(s(s(s(s(0)))), s(s(s(s(0))))).
f(s(s(s(0))), s(s(s(0)))).
f(s(s(0)), s(s(0))).
f(s(0), s(0)).

:- prop nat_num(X)
   #"@var{X} is a natural number.".
nat_num(0).
nat_num(s(X)) :- nat_num(X).


:- doc(equal/2,"Defines equality operator @op{==} between two natural numbers in Peano notation. @includedef{equal/2}\n").

:- pred equal(N1, N2) :: (nat_num(N1), nat_num(N2))
#"@pred{equal/2} will be true if both numbers are equal".

equal(0,0).
equal(s(N), s(N)) :-
   nat_num(N),
   equal(N,N).


:- doc(plus/3,"Defines sum operator @op{+} between two natural numbers in Peano notation. @includedef{plus/3}\n
   In case base, the sum of any number with 0, is the same number. The recursive call decrements the value of the first 
   operand to 0, reaching case base, which assigns the second operand to the result and, when returning, increments the 
   result as many times as recursive calls have been made.  ").

:- pred plus(Op1, Op2, Res) :: (nat_num(Op2), nat_num(Op1), nat_num(Res)) 
   #"@var{Res} is the sum of @var{Op1} and @var{Op2}".

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- nat_num(X), nat_num(Y), plus(X,Y,Z).


:- doc(minus/3,"Defines subtraction operator @op{-} between two natural numbers in Peano notation, using @pred{plus/3}. @includedef{minus/3}\n ").

:- pred minus(Op2, Op1, Res) :: (nat_num(Op2), nat_num(Op1), nat_num(Res))
   #"@var{Res} is the difference between @var{Op2} and @var{Op1}".
  
minus(X, Y, Z) :- plus(Z, Y, X).


:- doc(less/2,"Defines less than @op{>} operator between two natural numbers in Peano notation. @includedef{less/2}\n ").

:- pred less(N, M) :: (nat_num(N), nat_num(M))
#"@pred{less/2} will be true if @var{N} less than @var{M}".

less(0,s(_X)).
less(s(X),s(Y)) :- less(X,Y).


:- doc(div/3,"Defines division @op{/} between two natural numbers in Peano notation.  @includedef{div/3}\n 
 Division is viewed as successive subtractions from the dividend until it becomes 0 or the remainder. ").

div(0,_,0).
div(X, Y, s(0)) :- minus(X, Y, Z), less(Z, Y). 
div(X, Y, s(Q)) :- minus(X, Y, Z), div(Z, Y, Q).

:- pred div(Dividend, Divisor, Quotient) :: (nat_num(Dividend), nat_num(Divisor), nat_num(Quotient)).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% DEFINICIONES DE LAS SUPERFICIES

:- doc(basic_surface/1,"Defines a surface of cell charges represented by a list of lists. Internal
representation will be @tt{[List | [Lists]]}. @p
@begin{note}
@bf{Note:} @pred{basic_surface/1} must contains at least one non-empty horizontal line.
@end{note}
@p It is defined as: @includedef{basic_surface/1}\n").


:- prop basic_surface(CellList) 
#"@var{CellList} is a list of lists with charged cells.".

:- test basic_surface(S)
   : (S = [[0,++], [], [+,+++]]) + fails
   # "Cannot have blank lines.".

:- test basic_surface(S)
   : (S = [[0,1], [+++,+], [+,++]]) + fails
   # "Line contents must have cell charge values.".

:- test basic_surface(S)
   : (S = []) + fails.

:- test basic_surface(S)   
   : (S = [[]]) + fails
   # "Cannot be a list with a empty sublist".

:- test basic_surface(S)
   : (S = [[_]]) + not_fails
   # "Line must contain at least one cell.".

basic_surface([L]) :-
   my_list(L).

%basic_surface([[H|T]]) :-  
%   charge(H), 
%   my_list(T).
% OJO: SI descomentas esta parte, creas mas casos y posible bucle infinito con la misma solucion

basic_surface([L|S2]) :- 
   my_list(L), 
   basic_surface(S2).


:- doc(surface/1,"Defines a surface of charged cells, represented by a cell charge matrix. 
   @begin{note}
   @bf{Note:} @pred{surface/1} must contains at least one non-empty horizontal line.
   @end{note}
   @p It is defined as: @includedef{surface/1}\n").

:- prop surface(CellMatrix) 
   #"@var{CellMatrix} is a matrix with charged cells.".

:- test surface(S)
   : (S = [[0,++], [], [+,+++]]) + fails
   # "Cannot have blank lines.".

:- test surface(S)
   : (S = [[+++,1], [+++,+], [+,++]]) + fails
   # "Line contents must have cell charge values.".

:- test surface(S)
   : (S = [[++,+++], [0,+], [+,++]]) + not_fails.

:- test surface(S)
   : (S = [[]]) + fails
   # "Cannot be a list with empty sublist.".

:- test surface(S)
   : (S = [[_]]) + not_fails
   # "Line must contain at least one cell.".

:- test surface(S)
   : (S = [[_,_,_], [_,_], [_,_,_]]) + fails
   # "Lines must have same length.".



surface([L|L2]) :- 
   basic_surface([L|L2]), 
   mylength(L, Tam), 
   surface_acc([L|L2], Tam).

:- doc(surface_acc/2, "").

:- pred surface_acc(S, N) :: (surface(S), nat_num(N)).

surface_acc([L],Tam) :- mylength(L, Tam).
surface_acc([[H|T]|T2], Acc) :- 
   mylength([H|T],NewAcc), 
   equal(Acc,NewAcc), 
   surface_acc(T2,NewAcc).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES CON SUPERFICIES

:- doc(h_line/3,"Extracts a specific horizontal line from surface @includedef{h_line/3}\n").

:- pred h_line(S, I, R) :: (surface(S), nat_num(I), my_list(R)).

:- test h_line([[0,+,++], [+++,++++,+++++]], X, [+++,++++,+++++])
   : (X = s(s(0))) + not_fails.

:- test h_line([[0,+,++], [+++,++++,+++++]], X, _)
   : (X = s(s(s(0)))) + fails.

:- test h_line([[0,+,++], [+++,++++,+++++]],X, _)
   : (X = 0) + fails.

:- test h_line([[0,+,++], [+++,++++,+++++]], s(s(0)), X)
   : (X = [+++,++++,+++++]) + not_fails.

:- test h_line(X,I,C)
   : (X = []) + fails.

:- test h_line(X,I,C)
   : (X = [[]]) + fails.

h_line([L], s(0), L).   
h_line(S, N, C) :-
   get(S, N, C),
   surface(S).

:- doc(v_line/3, "Extracts a specific vertical line from surface @includedef{v_line/3}").

:- pred v_line(S, I, C) :: (surface(S), nat_num(I), my_list(C)).

:- test v_line([[0,+,++], [0,+,++]], X, [+,+])
   : (X = s(s(0))) + not_fails.

:- test v_line([[0,+,++], [0,+,++]], X, _)
   : (X = s(s(s(s(0))))) + fails.

:- test v_line([[0,+,++], [0,+,++]], X, _)
   : (X = 0) + fails.

:- test v_line([[0,+,++], [0,+,++]], s(s(0)), X)
   : (X = [+,+]) + not_fails.

:- test v_line(X,I,C)
   : (X = []) + fails.

:- test v_line(X,I,C)
   : (X = [[]]) + fails.

v_line([Fila], Indice, [Elemento]) :- get(Fila, Indice, Elemento).
v_line([Fila|Filas], Indice, [Elemento|Columna]) :-
   get(Fila, Indice, Elemento),
   v_line(Filas, Indice, Columna).

:- doc(v_lines/2, "Extracts all vertical lines. @includedef{v_lines/2}").

:- pred v_lines(S, C) : (surface(S)) => (surface(C)).
   

v_lines([L], L).
v_lines([L|S2], C) :-
   mylength(L, Tam),
   v_lines_aux([L|S2], Tam, C). 


:- pred v_lines_aux(S, T, C) :: (surface(S), nat_num(T), surface(C)).

v_lines_aux(S, s(0), [C]) :- v_line(S, s(0), C).
v_lines_aux(S, s(Index), Resto) :-
   v_lines_aux(S, Index, NewResto),
   v_line(S, s(Index), Coln), 
   myappend(NewResto, [Coln], Resto).
   
 /*  [[a,b,c][a,b,c][a,b,c]]

   [[a,a,a]]  N = 1
   [[a,a,a]] ++ [b,b,b] = [[a,a,a][b,b,b]] N > 1
   [[a,a,a][b,b,b]] ++ [c,c,c] = [[a,a,a][b,b,b][c,c,c]]

   Index = s(0) NewResto = [[a,a,a]]
   Coln = [b,b,b]
   [[a,a,a]] ++ [b,b,b] = [[a,a,a][b,b,b]]

   Index = s(s(0)) Resto = [[a,a,a][b,b,b]]
   Coln = [c,c,c]
   [[a,a,a][b,b,b]] ++ [c,c,c] = [[a,a,a][b,b,b][c,c,c]]

   Index = s(s(s(0))) Resto = [[a,a,a][b,b,b][c,c,c]]
*/

:- doc(h_sum/2, "Sum elements of a horizontal line. @includedef{h_sum/2}\n").

:- pred h_sum(R, T) :: (my_list(R), nat_num(T)).

h_sum([C], N) :- f(C, N).
h_sum([H|T], Suma) :-
   h_sum(T, SumaResto),
   f(H, N),
   plus(N, SumaResto, Suma).

:- pred total_charge(S, T) :: (surface(S), nat_num(T))
   #"@var{T} is the sum of all elements in a surface.".

:- test total_charge([[0,+],[++,+++],[++++,+++++]], X)
   => X = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))).

:- test total_charge(X, _)
   : (X = [[0,+],[++,3],[++++,+++++]]) + fails.

:- test total_charge([[0,+],[++,+++],[++++,+++++]], X)
   : (X = 15) + fails.

:- test total_charge(X, _)
   : (X = [[_,_],[_,_,_],[_,_]]) + fails.

:- test total_charge(X, _)
   : (X = [[]]) + fails.

:-test total_charge(X,_)
   : (X = []) + fails.

total_charge(S, T) :-
   total_charge_aux(S,T), 
   surface(S).

:- pred total_charge_aux(S, T) :: (surface(S), nat_num(T)).

total_charge_aux([],0).
total_charge_aux([L|S2], T) :-
   total_charge_aux(S2, SumaResto),
   h_sum(L, SumaFila),
   plus(SumaFila, SumaResto, T).

:- doc(total_cells/2, "Calculates the number of elements of a surface. @includedef{total_cells/2}\n").

:- pred total_cells(S, N) :: (surface(S), nat_num(N)).

total_cells([L], N) :- mylength(L,N).
total_cells([L|S2], Total) :-
   total_cells(S2, TotalResto),
   mylength(L, Esta),
   plus(Esta, TotalResto, Total).

:- doc(average_charge/2, "Calculates average cell charge value from surface. @includedef{average_charge/2}\n").

:- pred average_charge(S, A) :: (surface(S), nat_num(A)).

average_charge([[]], _).
average_charge(S, A) :-
   total_charge(S, M),
   total_cells(S, T), 
   div(M, T, A).

%------------------------------------------------------------------------------------------------------------------------%

% FIN CODIGO
