:- module(_,_,[pure,assertions,regtypes]).
%:- module(_,_,[assertions,regtypes]).
% :- module(_,_,[]).           % For pure LP, depth-first search rule
%:- module(_,_,['sr/bfall']).   % For pure LP, breadth-first search rule, all predicates


:- doc(title, "Pure Logic Programming: Practise 1").
:- doc(author, "Eduardo Gil Alba, z170238").

:- doc(module, "
@section{Introduction}
The task at hand involves validating representations of statically loaded surfaces with variable load values assigned to 
discrete points. To facilitate analysis, we adopt several simplifications. Firstly, we constrain charges to predefined values 
according to the following predicate: @includedef{charge/1} Here, each charge is denoted by the number of '+' symbols, e.g., 
'++++' signifies a charge of four units. @p
Furthermore, we discretize the two-dimensional surface, conceptualizing it as a grid of cells where each cell can hold one of 
the predefined charge values. An illustrative example of such a surface is:
@begin{verbatim}
   +++    +++++++    0      +    ++++    0
 +++++++    +++      0      +    ++++    0
   +++       0    +++++++   +     0     ++++
   +++    +++++++    0    ++++    +      0
+++++++      0      +++     +    ++++    0
   +++    +++++++   +++     +     0      +
 +++++++    +++      0      +    ++++    0
@end{verbatim}
For representation purposes, we employ a list of lists. The inner lists delineate cells horizontally, while the outer list 
groups cells by rows, with each element representing a load value. Thus, the above surface is structured as follows:
@begin{verbatim}
[ [ +++ , +++++++ , 0 , + , ++++ , 0 ],
[ +++++++ , +++ , 0 , + , ++++ , 0 ],
[ +++ , 0 , +++++++ , + , 0 , ++++ ],
[ +++ , +++++++ , 0 , ++++ , + , 0 ],
[ +++++++ , 0 , +++ , + , ++++ , 0 ],
[ +++ , +++++++ , +++ , + , 0 , + ],
[ +++++++ , +++ , 0 , + , ++++ , 0 ] ]
@end{verbatim}
Surfaces can have an arbitrary number of cells in both dimensions. @p
In order to accomplish this, we define two types of surfaces, both containing load values. We establish @prop{basic_surface/1}
which must consist of at least one line, with each line containing at least one cell. It is defined by:
@includedef{basic_surface/1} 
Additionally, we define @prop{surface/1} as above but in addition must ensure that all lines possess the same number of cells. It is defined by:
@includedef{surface/1}
@includedef{surface_acc/2}


@subsection{Some examples of use:}
@begin{enumerate}
@item Check if a @prop{basic_surface/1} is correct:
@begin{verbatim}
  ?- basic_surface([[+++,+++++++,0,+], 
              [+++++++,+++,0,+,++++], 
              [+++,0], 
              [+++,+++++++,0], 
              [+++++++,0,+++,+,++++], 
              [+++], 
              [+++++++,+++,0]]).

  yes
  ?- 
@end{verbatim}
@item Check if a @prop{surface/1} is correct:
@begin{verbatim}
  ?- surface([[+++,+++++++,0,+,++++,0], 
              [+++++++,+++,0,+,++++,0], 
              [+++,0,+++++++,+,0,++++], 
              [+++,+++++++,0,++++,+,0], 
              [+++++++,0,+++,+,++++,0], 
              [+++,+++++++,+++,+,0,+], 
              [+++++++,+++,0,+,++++,0]]).

  yes
  ?- 
@end{verbatim}
@end{enumerate}").

:- doc(author_data/4,"Defines authors in Deliverit system.").
:- prop author_data(Surname1, Surname2, Name, ID) 
#"@var{Surname1} is your first surname.\n
@var{Surname2} is your second surname.\n
@var{Name} is your name.\n
@var{ID} is university identifier.".
author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

:- doc(charge/1,"Defines possible load values. @includedef{charge/1}\n").

:- prop charge(C)
   #"@var{C} is a load value".

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

my_list([H|T]) :- 
   charge(H),
   my_list(T).

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES DE LISTA

:- doc(my_length/2, "Calculates the size of a list. @includedef{my_length/2}\n").

:- pred my_length(L, N) :: (my_list(L), nat_num(N)).

my_length([],0).
my_length([_|T],s(N)) :-
   my_length(T,N).

:- doc(get/2, "Extracts a specific element from a list. @includedef{get/2}\n").

:- pred get(L, I, E) :: (my_list(L), nat_num(I), charge(E)).

get([Elem|_], s(0), Elem).
get([_|Rest], s(Index), Elem) :-
   get(Rest, Index, Elem).

:- doc(my_append/3, "").

:- pred my_append(L1, L2, R) :: (my_list(L1), my_list(L2), my_list(R)).

my_append([],L,L).
my_append([X|Xs],Ys,[X|Zs]) :- 
    my_append(Xs,Ys,Zs).

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

plus(0,Y,Y) :- nat_num(Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).


:- doc(minus/3,"Defines subtraction operator @op{-} between two natural numbers in Peano notation, using @pred{plus/3}. @includedef{minus/3}\n ").

:- pred minus(Op2, Op1, Res) :: (nat_num(Op2), nat_num(Op1), nat_num(Res))
   #"@var{Res} is the difference between @var{Op2} and @var{Op1}".
  
minus(X, Y, Z) :- plus(Z, Y, X).


:- doc(less/2,"Defines less than @op{>} operator between two natural numbers in Peano notation. @includedef{less/2}\n ").

:- pred less(N, M) :: (nat_num(N), nat_num(M))
#"@pred{less/2} will be true if @var{N} less than @var{M}".

less(0,s(X)) :- nat_num(X).
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
   : (S = []) + fails
   #"Cannot be an empty list.".

:- test basic_surface(S)   
   : (S = [[]]) + fails
   # "Cannot be a list with a empty sublist".

:- test basic_surface(S)
   : (S = [[_]]) + not_fails
   # "Line must contain at least one cell.".

:- test basic_surface(S)
   : (S = [[_,_,_],[_,_],[_]]) + not_fails
   #"Lines could have different sizes.".

basic_surface([L]) :-
   my_list(L).

%basic_surface([[H|T]]) :-  
%   charge(H), 
%   my_list(T).
% OJO: SI descomentas esta parte, creas mas casos y posible bucle infinito con la misma solucion

basic_surface([L|S2]) :- 
   my_list(L), 
   basic_surface(S2).


:- doc(surface/1,"Defines a surface of charged cells, represented by a cell charge matrix. @p
   @begin{note}
   @bf{Note:} @pred{surface/1} must contains at least one non-empty horizontal line.
   @end{note}
   @p It is defined as: @includedef{surface/1}\n").

:- prop surface(CellMatrix) 
   #"@var{CellMatrix} is a matrix with charged cells.".

:- test surface(S)
   : (S = [[++,+++], [0,+], [+,++]]) + not_fails
   #"Call surface correctly.".

:- test surface(S)
   : (S = [[0,++], [], [+,+++]]) + fails
   # "Cannot have blank lines.".

:- test surface(S)
   : (S = [[+++,1], [+++,+], [+,++]]) + fails
   # "Line contents must have cell charge values.".

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
   my_length(L, Tam), 
   surface_acc([L|L2], Tam).

:- doc(surface_acc/2, "").

:- pred surface_acc(S, N) :: (surface(S), nat_num(N)).

surface_acc([L],Tam) :- my_length(L, Tam).
surface_acc([[H|T]|T2], Acc) :- 
   my_length([H|T],NewAcc), 
   equal(Acc,NewAcc), 
   surface_acc(T2,NewAcc).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES CON SUPERFICIES

:- doc(h_line/3,"Extracts a specific horizontal line from surface @includedef{h_line/3}\n").

:- pred h_line(S, I, R) :: (surface(S), nat_num(I), my_list(R)).

:- test h_line(S, I, R)
   : (S = [[0,+,++], [+++,++++,+++++]], I  = s(0))
   => (R = [0,+,++]) + not_fails
   #"Get horizontal line correctly(1).".

:- test h_line(S, I, R)
   : (S = [[0,+,++], [+++,++++,+++++]], I  = s(s(0)))
   => (R = [+++,++++,+++++]) + not_fails
   #"Get horizontal line correctly(2).".

:- test h_line(S, I, R)
   : (S = [[0,+,++], [+++,++++,+++++]], R = [+++,++++,+++++])
   => (I = s(s(0))) + not_fails
   #"Get horizontal line index correctly.".

:- test h_line(_,I, _)
   : (I = 0) + fails
   #"Get non-existing horizontal line(1)".

:- test h_line(S, I, _)
   : (S = [[0,+,++], [+++,++++,+++++]], I = s(s(s(0)))) + fails
   #"Get non-existing horizontal line(2).".

:- test h_line(S,_,_)
   : (S = []) + fails
   #"Cannot be an empty surface.".


h_line([L], s(0), L).   
h_line(S, N, C) :-
   get(S, N, C),
   surface(S).

:- doc(v_line/3, "Extracts a specific vertical line from surface @includedef{v_line/3}").

:- pred v_line(S, I, C) :: (surface(S), nat_num(I), my_list(C)).

:- test v_line(S, I, C)
   : (S = [[0,+,++], [0,+,++]], I = s(s(0)))
   => (C = [+,+])
   #"Get column correctly".

:- test v_line(S, I, C)
   : (S = [[0,+,++], [0,+,++]], C = [+,+])
   => (I = s(s(0)))
   #"Get index from column correctly".

:- test v_line(S, I, _)
   : (S = [[0,+,++], [0,+,++]], I = 0) + fails
   #"Get non-existing vertical line(1)".

:- test v_line(S, I, _)
   : (S = [[0,+,++], [0,+,++]], I = s(s(s(s(0))))) + fails
   #"Get non-existing vertical line(2)".

:- test v_line(S,_,_)
   : (S = []) + fails
   #"Cannot be an empty surface.".

:- test v_line(S,_,_)
   : (S = [[]]) + fails
   #"Surface must have at least one cell".

v_line([Fila], Indice, [Elemento]) :- get(Fila, Indice, Elemento).
v_line([Fila|Filas], Indice, [Elemento|Columna]) :-
   get(Fila, Indice, Elemento),
   v_line(Filas, Indice, Columna).

:- doc(v_lines/2, "Extracts all vertical lines. @includedef{v_lines/2}").

:- pred v_lines(S, C) : (surface(S)) => (surface(C)).
   

:- test v_lines(S, C)
   : (S = [[0,+,++],[0,+,++],[0,+,++]])
   => (C = [[0,0,0],[+,+,+],[++,++,++]])
   #"Test with square matrix".

:- test v_lines(S, C)
   : (S = [[0,+,++],[0,+,++]])
   => (C = [[0,0],[+,+],[++,++]])
   #"Test with rectangular matrix".

v_lines([L], L).
v_lines([L|S2], C) :-
   my_length(L, Tam),
   v_lines_aux([L|S2], Tam, C). 


:- pred v_lines_aux(S, T, C) :: (surface(S), nat_num(T), surface(C)).

v_lines_aux(S, s(0), [C]) :- v_line(S, s(0), C).
v_lines_aux(S, s(Index), Resto) :-
   v_lines_aux(S, Index, NewResto),
   v_line(S, s(Index), Coln), 
   my_append(NewResto, [Coln], Resto).
   
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

:- test total_charge([[0,+],[++,+++],[++++,+++++]], T)
   => T = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))).

:- test total_charge(S, _)
   : (S = [[0,+],[++,3],[++++,+++++]]) + fails.

:- test total_charge(S, _)
   : (S = [[]]) + fails.

:- test total_charge(S, _)
   : (S = [[_,_,_],[_,_],[_]]) + fails.


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

total_cells([L], N) :- my_length(L,N).
total_cells([L|S2], Total) :-
   total_cells(S2, TotalResto),
   my_length(L, Esta),
   plus(Esta, TotalResto, Total).

:- doc(average_charge/2, "Calculates average cell charge value from surface. @includedef{average_charge/2}\n").

:- pred average_charge(S, A) :: (surface(S), nat_num(A)).

:- test average_charge(S, A)
   : (S = [[++,++],[++,++],[++,++]])
   => (A = s(s(0)))
   #"Get average charge correctly(1).".

:- test average_charge(S, A)
   : (S = [[+,++],[+,++],[+,++]])
   => (A = s(0))
   #"Get average charge correctly(2).".

:- test average_charge(S, A)
   : (S = [[++,+++,+++++++], [+,++,++],[++,+++,+],[++,++,++]], A = s(s(s(s(s(s(0))))))) + fails
   #"Round to lower number.".

:- test average_charge(S, _)
   : (S = [[],[],[]]) + fails.


average_charge([[]], _).
average_charge(S, A) :-
   total_charge(S, M),
   total_cells(S, T), 
   div(M, T, A).

%------------------------------------------------------------------------------------------------------------------------%

% FIN CODIGO
