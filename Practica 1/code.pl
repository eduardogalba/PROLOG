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
As you can see, an auxiliary predicate has been employed, which accepts the size as a term. In each recursion step, it verifies 
the length of the horizontal lines.
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
@end{enumerate}

@section{Operations with surfaces}
We define certain operations that can be performed on surfaces.

@subsection{h_line(S,N,C)}
@var{C} is the @var{N}th horizontal line of the surface @var{S} @p
@var{N} is a natural number in Peano notation.The horizontal lines are represented as lists with load values. 
To facilitate this objective, an auxiliary predicate has been used, tasked with extracting an element from a list.
 Specifically, it retrieves a list from within a list of lists.

@subsubsection{Examples}
@begin{enumerate}
@item Lines begin from s(0) : 
@begin{verbatim}
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], 0, L).

no
?-
@end{verbatim}
@item Extract 4th horizontal line: 
@begin{verbatim}
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], s(s(s(s(0)))), L).

L = [+,+++,0] ? 
yes
?-
@end{verbatim}
@item Returns position from line given:
@begin{verbatim}
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [+,+++,0]).

N = s(0) ? ;
N = s(s(0)) ? ;
N = s(s(s(s(0)))) ? ; 
yes
?-
@end{verbatim}
@item Passing non-existing line:
@begin{verbatim}
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [++,+++++,++++]).

no
?-
@end{verbatim}
@end{enumerate}

@subsection{v_line(S,N,C)}
@var{C} is the list of the Nth cells of all horizontal lines of the surface @var{S}. @p
@var{N} is a natural number in Peano notation. Similarly to the aforementioned predicate, an auxiliary predicate has been used, 
tasked with extracting an element from a list. Specifically, it retrieves a list from within a list of lists.
@subsubsection{Examples}
@begin{enumerate}
@item Lines begin from s(0) : 
@begin{verbatim}
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], 0, C).

no
?-
@end{verbatim}
@item Extract 2nd vertical line:
@begin{verbatim}
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], s(s(0)), C).

C = [+++,+++,0,+++,0,+,++] ? 
yes
?-
@end{verbatim}
@item Returns position from line given:
@begin{verbatim}
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [0,0,++,0,++,++,0]).

N = s(s(s(0))) ? 
yes
?-
@end{verbatim}
@item Passing non-existing line:
@begin{verbatim}
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [++,0,+,+++++,++++,++,0]).

no
?-
@end{verbatim}
@end{enumerate}

@subsection{v_lines(S, C)}
@var{C} is the list of vertical lines of cells on the surface @var{S}. @p
It utilizes an auxiliary predicate which, during each recursion, extracts the vertical line and concatenates it 
to the final list.
@subsubsection{Examples}
@begin{enumerate}
@item Extract all vertical lines:
@begin{verbatim}
v_lines([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], C).

C = [[+,+,++,+,++,+,+],[+++,+++,0,+++,0,+,++],[0,0,++,0,++,++,0]] ?
yes
?-
@end{verbatim}
@item Surface given with empty horizontal line:
@begin{verbatim}
v_lines([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], C).

no
?-
@end{verbatim}
@end{enumerate}

@subsection{total_charge(S,T)}
@var{T} is the sum of all load values in the surface @var{S}. @p
It employs an auxiliary predicate that utilizes another predicate to calculate the sum of all loads within a horizontal line, 
with each recursive step contributing to the total sum.

@subsubsection{Examples}
@begin{enumerate}
@item Calculates total charge:
@begin{verbatim}
?- total_charge([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], T).

T = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))))))))))))))) ?  
yes
?-
@end{verbatim}
@item It does not allow @prop{basic_surface/1}:
@begin{verbatim}
?- total_charge([[+++,+++++++,0,+], 
              [+++++++,+++,0,+,++++], 
              [+++,0], 
              [+++,+++++++,0], 
              [+++++++,0,+++,+,++++], 
              [+++], 
              [+++++++,+++,0]], T).
no
?-
@end{verbatim}
@end{enumerate}

@subsection{average_charge(S,A)}
@var{A} is the average of all load valuesin the surface @var{S}.@p
This entails summing all the loads and dividing by the total number of cells. The result must be rounded by truncation, i.e. returning the natural prior.
For this purpose, the aforementioned predicate calculates the total sum of the load values, along with an auxiliary predicate that 
computes the total number of elements in the surface. Additionally, an integer division predicate is employed.
@subsubsection{Examples}
@begin{enumerate}

@item Calculates average charge from example:
@begin{verbatim}
?- average_charge([[+++,+++++++,0,+,++++,0], 
              [+++++++,+++,0,+,++++,0], 
              [+++,0,+++++++,+,0,++++], 
              [+++,+++++++,0,++++,+,0], 
              [+++++++,0,+++,+,++++,0], 
              [+++,+++++++,+++,+,0,+], 
              [+++++++,+++,0,+,++++,0]], A).

A = s(s(0)) ? 
yes
?-
@end{verbatim}

@item Surface given with wrong load values:
@begin{verbatim}
?- average_charge([[3,7,0,1,4,0], 
              [7,3,0,1,4,0], 
              [3,0,7,1,0,4], 
              [3,7,0,4,1,0], 
              [7,0,3,1,4,0], 
              [3,7,3,1,0,1], 
              [7,3,0,1,4,0]], A).

no
?-
@end{verbatim}
@end{enumerate}
").

:- doc(author_data/4,"Defines authors in Deliverit system.").

author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

:- doc(charge/1,"Defines possible load values. @p 
It is defined as:  @includedef{charge/1}\n").

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

:- doc(my_list/1,"Defines a list with load values. @p
It is defined as:  @includedef{my_list/1}\n
The predicate @pred{my_list/1} is called recursively, checking that all elements are charges and that it is a list structure. ").

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
   #"@var{L} is a list with load values.".

my_list([H]) :- charge(H).

my_list([H|T]) :- 
   charge(H),
   my_list(T).

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES DE LISTA

:- doc(my_length/2, "Calculates the size of a list. It is defined by: @includedef{my_length/2}\n").

:- pred my_length(L, N) :: (my_list(L), nat_num(N)).

my_length([],0).
my_length([_|T],s(N)) :-
   my_length(T,N).

:- doc(get/3, "Extracts Nth element from a list. It is defined by: @includedef{get/3}\n").

:- pred get(L, I, E) :: (my_list(L), nat_num(I), charge(E)).

get([Elem|_], s(0), Elem).
get([_|Rest], s(Index), Elem) :-
   get(Rest, Index, Elem).

:- doc(my_append/3, "Concatenate two lists. It is defined by: @includedef{my_append/3}").

:- pred my_append(L1, L2, R) :: (my_list(L1), my_list(L2), my_list(R)).

my_append([],L,L).
my_append([X|Xs],Ys,[X|Zs]) :- 
    my_append(Xs,Ys,Zs).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES ARITMETICAS
:- doc(f/2,"Define equivalences between load values and natural numbers in Peano notation. In order to be able to perform 
arithmetic operations. @p
It is defined by: @includedef{f/2}\n ").

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

/*
f(s(s(s(s(s(s(s(0))))))), s(s(s(s(s(s(s(0)))))))).
f(s(s(s(s(s(s(0)))))), s(s(s(s(s(s(0))))))).
f(s(s(s(s(s(0))))), s(s(s(s(s(0)))))).
f(s(s(s(s(0)))), s(s(s(s(0))))).
f(s(s(s(0))), s(s(s(0)))).
f(s(s(0)), s(s(0))).
f(s(0), s(0)).
*/

:- doc(nat_num/1, "Defines natural numbers in Peano notation. @p 
It is defined as: @includedef{nat_num/1}").

:- prop nat_num(X)
   #"@var{X} is a natural number.".
nat_num(0).
nat_num(s(X)) :- nat_num(X).


:- doc(equal/2,"Defines equality operator @op{==} between two natural numbers in Peano notation. 
@p It is defined by: @includedef{equal/2}\n").

:- pred equal(N1, N2) :: (nat_num(N1), nat_num(N2))
#"@pred{equal/2} will be true if both numbers are equal".

equal(0,0).
equal(s(N), s(N)) :-
   nat_num(N),
   equal(N,N).


:- doc(my_plus/3,"Defines sum operator @op{+} between two natural numbers in Peano notation. 
@p It is defined by: @includedef{my_plus/3}\n
   In case base, the sum of any number with 0, is the same number. The recursive call decrements the value of the first 
   operand to 0, reaching case base, which assigns the second operand to the result and, when returning, increments the 
   result as many times as recursive calls have been made.  ").

:- pred my_plus(Op1, Op2, Res) :: (nat_num(Op2), nat_num(Op1), nat_num(Res)) 
   #"@var{Res} is the sum of @var{Op1} and @var{Op2}".

my_plus(0,Y,Y) :- nat_num(Y).
my_plus(s(X),Y,s(Z)) :- my_plus(X,Y,Z).


:- doc(my_minus/3,"Defines subtraction operator @op{-} between two natural numbers in Peano notation, using @pred{my_plus/3}. 
@p It is defined by: @includedef{my_minus/3}\n ").

:- pred my_minus(Op2, Op1, Res) :: (nat_num(Op2), nat_num(Op1), nat_num(Res))
   #"@var{Res} is the difference between @var{Op2} and @var{Op1}".
  
my_minus(X, Y, Z) :- my_plus(Z, Y, X).


:- doc(my_less/2,"Defines less than @op{>} operator between two natural numbers in Peano notation.
@p It is defined by:  @includedef{my_less/2}\n ").

:- pred my_less(N, M) :: (nat_num(N), nat_num(M))
#"@pred{my_less/2} will be true if @var{N} my_less than @var{M}".

my_less(0,s(X)) :- nat_num(X).
my_less(s(X),s(Y)) :- my_less(X,Y).


:- doc(div/3,"Defines division @op{/} between two natural numbers in Peano notation. 
@p It is defined by:  @includedef{div/3}\n 
 Division is viewed as successive subtractions from the dividend until it becomes 0 or the remainder. ").

div(0,_,0).
div(X, Y, s(0)) :- my_minus(X, Y, Z), my_less(Z, Y). 
div(X, Y, s(Q)) :- my_minus(X, Y, Z), div(Z, Y, Q).

:- pred div(Dividend, Divisor, Quotient) :: (nat_num(Dividend), nat_num(Divisor), nat_num(Quotient)).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% DEFINICIONES DE LAS SUPERFICIES

:- doc(basic_surface/1,"Defines a surface of load values. @p
@begin{note}
@bf{Note:} @pred{basic_surface/1} must contains at least one non-empty horizontal line.
@end{note}
@p It is defined as: @includedef{basic_surface/1}\n").


:- prop basic_surface(CellList) 
#"@var{CellList} is a list of lists with load cells.".

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


:- doc(surface/1,"Defines a surface of load values, represented by a load value matrix. @p
   @begin{note}
   @bf{Note:} @pred{surface/1} must contains at least one non-empty horizontal line.
   @end{note}
   @p It is defined as: @includedef{surface/1}\n").

:- prop surface(CellMatrix) 
   #"@var{CellMatrix} is a load value matrix.".

:- test surface(S)
   : (S = [[++,+++], [0,+], [+,++]]) + not_fails
   #"Call surface correctly.".

:- test surface(S)
   : (S = [[0,++], [], [+,+++]]) + fails
   # "Cannot have blank lines.".

:- test surface(S)
   : (S = [[+++,1], [+++,+], [+,++]]) + fails
   # "Line contents must have load values.".

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

:- doc(surface_acc/2, "Checks length in all horizontal lines. @p
It is defined by: @includedef{surface_acc/2}").

:- pred surface_acc(S, N) :: (surface(S), nat_num(N)).

surface_acc([L],Tam) :- my_length(L, Tam).
surface_acc([[H|T]|T2], Acc) :- 
   my_length([H|T],NewAcc), 
   equal(Acc,NewAcc), 
   surface_acc(T2,NewAcc).

%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES CON SUPERFICIES

:- doc(h_line/3,"Extracts Nth horizontal line from surface. 
@p It is defined by: @includedef{h_line/3}\n").

:- pred h_line(S, N, R) :: (surface(S), nat_num(N), my_list(R)).

:- test h_line(S, N, R)
   : (S = [[0,+,++], [+++,++++,+++++]], N  = s(0))
   => (R = [0,+,++]) + not_fails
   #"Get horizontal line correctly(1).".

:- test h_line(S, N, R)
   : (S = [[0,+,++], [+++,++++,+++++]], N  = s(s(0)))
   => (R = [+++,++++,+++++]) + not_fails
   #"Get horizontal line correctly(2).".

:- test h_line(S, N, R)
   : (S = [[0,+,++], [+++,++++,+++++]], R = [+++,++++,+++++])
   => (N = s(s(0))) + not_fails
   #"Get horizontal line index correctly.".

:- test h_line(_,N, _)
   : (N = 0) + fails
   #"Get non-existing horizontal line(1).".

:- test h_line(S, N, _)
   : (S = [[0,+,++], [+++,++++,+++++]], N = s(s(s(0)))) + fails
   #"Get non-existing horizontal line(2).".

:- test h_line(S,_,_)
   : (S = []) + fails
   #"Cannot be an empty surface.".


h_line([L], s(0), L).   
h_line(S, N, C) :-
   get(S, N, C),
   surface(S).

:- doc(v_line/3, "Extracts Nth vertical line from surface. 
@p It is defined by:  @includedef{v_line/3}").

:- pred v_line(S, N, C) :: (surface(S), nat_num(N), my_list(C)).

:- test v_line(S, N, C)
   : (S = [[0,+,++], [0,+,++]], N = s(s(0)))
   => (C = [+,+])
   #"Get vertical line correctly.".

:- test v_line(S, N, C)
   : (S = [[0,+,++], [0,+,++]], C = [+,+])
   => (N = s(s(0)))
   #"Get index from vertical line correctly.".

:- test v_line(S, N, _)
   : (S = [[0,+,++], [0,+,++]], N = 0) + fails
   #"Get non-existing vertical line(1).".

:- test v_line(S, N, _)
   : (S = [[0,+,++], [0,+,++]], N = s(s(s(s(0))))) + fails
   #"Get non-existing vertical line(2).".

:- test v_line(S,_,_)
   : (S = []) + fails
   #"Cannot be an empty surface.".

:- test v_line(S,_,_)
   : (S = [[]]) + fails
   #"Surface must have at least one cell.".

v_line([Fila], Indice, [Elemento]) :- get(Fila, Indice, Elemento).
v_line([Fila|Filas], Indice, [Elemento|Columna]) :-
   get(Fila, Indice, Elemento),
   v_line(Filas, Indice, Columna).

:- doc(v_lines/2, "Extracts all vertical lines from a surface. 
@p It is defined by: @includedef{v_lines/2}").

:- pred v_lines(S, C) : (surface(S)) => (surface(C)).
   

:- test v_lines(S, C)
   : (S = [[0,+,++],[0,+,++],[0,+,++]])
   => (C = [[0,0,0],[+,+,+],[++,++,++]])
   #"Test with square matrix.".

:- test v_lines(S, C)
   : (S = [[0,+,++],[0,+,++]])
   => (C = [[0,0],[+,+],[++,++]])
   #"Test with rectangular matrix.".

:- test v_lines(S, C)
   : (S = [[0,+,++],[0,+,++],[0,+,++],[0,+,++]])
   => (C = [[0,0,0,0],[+,+,+,+],[++,++,++,++]])
   #"Extracts all vertical lines (1).".

:- test v_lines(S, C)
   : (S = [[0,+],[0,+],[0,+],[0,+],[0,+],[0,+],[0,+]])
   => (C = [[0,0,0,0,0,0,0],[+,+,+,+,+,+,+]])
   #"Extracts all vertical lines (2).".

:- test v_lines(S, C)
   : (S = [[0,+,++],[0,+],[0]]) + fails
   #"Test with basic surface.".

:- test v_lines(S, C)
   : (S = []) + fails
   #"Test with empty surface.".

v_lines([L], L).
v_lines([L|S2], C) :-
   my_length(L, Tam),
   v_lines_aux([L|S2], Tam, C). 


:-doc(v_lines_aux/3, "Builds the list of vertical lines @p 
It is defined by: @includedef{v_lines_aux/3}").

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

:- doc(h_sum/2, "Sum elements of a horizontal line. 
@p It is defined by:  @includedef{h_sum/2}\n").

:- pred h_sum(R, T) :: (my_list(R), nat_num(T)).

h_sum([C], N) :- f(C, N).
h_sum([H|T], Suma) :-
   h_sum(T, SumaResto),
   f(H, N),
   my_plus(N, SumaResto, Suma).

:- doc(total_charge/2, "Sum all load values of a surface. @p
It is defined by: @includedef{total_charge/2}").

:- pred total_charge(S, T) :: (surface(S), nat_num(T))
   #"@var{T} is the sum of all elements in a surface.".

:- test total_charge([[0,+],[++,+++],[++++,+++++]], T)
   => (T = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))))
   #"Calculate the total number of charges (1).".

:- test total_charge(S, T)
   : (S = [[0,+],[0,+],[0,+],[0,+],[0,+],[0,+],[0,+]])
   => (T = s(s(s(s(s(s(s(0))))))))
   #"Calculate the total number of charges (2).".

:- test total_charge(S, T)
   : (S = [[0,0,0],[0,0,0],[0,0,0]])
   => (T = 0)
   #"Surface with all 0s.".

:- test total_charge(S, _)
   : (S = [[0,+],[++,3],[++++,+++++]]) + fails
   #"Surface wrong load values.".

:- test total_charge(S, T)
   : (S = [[0,+],[++,+++],[+,++]], T = s(s(s(s(s(0)))))) + fails
   #"Incorrect number of total.".

:- test total_charge(S, _)
   : (S = [[]]) + fails
   #"Empty surface.".

:- test total_charge(S, _)
   : (S = [[_,_,_],[_,_],[_]]) + fails
   #"Must be a surface.".


total_charge(S, T) :-
   total_charge_aux(S,T), 
   surface(S).

:- doc(total_charge_aux/2, "Sum of all charges for each horizontal list @p
It is defined by: @includedef{total_charge_aux/2}").

:- pred total_charge_aux(S, T) :: (surface(S), nat_num(T)).

total_charge_aux([],0).
total_charge_aux([L|S2], T) :-
   total_charge_aux(S2, SumaResto),
   h_sum(L, SumaFila),
   my_plus(SumaFila, SumaResto, T).

:- doc(total_cells/2, "Calculates the number of elements of a surface. 
@p It is defined by:  @includedef{total_cells/2}\n").

:- pred total_cells(S, N) :: (surface(S), nat_num(N)).

total_cells([L], N) :- my_length(L,N).
total_cells([L|S2], Total) :-
   total_cells(S2, TotalResto),
   my_length(L, Esta),
   my_plus(Esta, TotalResto, Total).

:- doc(average_charge/2, "Calculates average load value from surface. @p 
It is defined by: @includedef{average_charge/2}\n").

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
   : (S = [[+],[+],[+],[+],[+],[+],[+]])
   => (T = s(0))
   #"Get average charge correctly(3).".

:- test average_charge(S, A)
   : (S = [[0,0,0],[0,0,0],[0,0,0]])
   => (A = 0)
   #"Surface with all 0s.".

:- test average_charge(S, A)
   : (S = [[++,+++,+++++++], [+,++,++],[++,+++,+],[++,++,++]], A = s(s(s(s(s(s(0))))))) + fails
   #"Round to natural prior.".

:- test average_charge(S, _)
   : (S = [[0,+],[],[++,++]]) + fails
   #"Surface with empty horizontal line.".

:- test average_charge(S, _)
   : (S = []) + fails
   #"Empty surface.".

average_charge([[]], _).
average_charge(S, A) :-
   total_charge(S, M),
   total_cells(S, T), 
   div(M, T, A).

%------------------------------------------------------------------------------------------------------------------------%

% FIN CODIGO
