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

my_list([+++++++]).
my_list([++++++]).
my_list([+++++]).
my_list([++++]).
my_list([+++]).
my_list([++]).
my_list([+]).
my_list([0]).
%my_list([]).

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

myappend([],L,L).
myappend([X|Xs],Ys,[X|Zs]) :- 
    myappend(Xs,Ys,Zs).

reverse([],[]).
reverse([X|Xs],Ys) :- 
    reverse(Xs,Zs),
    myappend(Zs,[X],Ys).


%------------------------------------------------------------------------------------------------------------------------%

%------------------------------------------------------------------------------------------------------------------------%

% OPERACIONES ARITMETICAS
:- doc(f/1,"Defines equivalences between charges values and natural numbers in Peano notation, in order to be able to perform 
arithmetic operations. @includedef{f/2}\n ").

f(+++++++, s(s(s(s(s(s(s(0)))))))).
f(++++++, s(s(s(s(s(s(0))))))).
f(+++++, s(s(s(s(s(s(s(0)))))))).
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

:- prop plus(Op1, Op2, Res)
#"@var{Op1} is a natural number.\n
@var{Op2} is a natural number.\n 
@var{Res} is the result.".

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).


:- doc(minus/3,"Defines subtraction operator @op{-} between two natural numbers in Peano notation, using @pred{plus/3}. @includedef{minus/3}\n ").

:- prop minus(Op2, Op1, Res)
#"@var{Op2} is a natural number. \n
@var{Op1} is a natural number. \n
@var{Res} is the result.".

minus(X, Y, Z) :- plus(Z, Y, X).


:- doc(less/2,"Defines less than @op{>} operator between two natural numbers in Peano notation. @includedef{less/2}\n ").

:- prop less(N, M)
#"@var{N} is a natural number \n
@var{M} is a natural number \n
@pred{less/2} will be true wheter @var{N} less than @var{M}".

less(0,s(_X)).
less(s(X),s(Y)) :- less(X,Y).

less_or_equal(0,_).
less_or_equal(s(X),s(Y)) :- less_or_equal(X,Y).

:- doc(div/3,"Defines division @op{/} between two natural numbers in Peano notation.  @includedef{div/3}\n 
 Division is viewed as successive subtractions from the dividend until it becomes 0 or the remainder. ").

div(0,_,0).
div(X, Y, s(0)) :- minus(X, Y, Z), less(Z, Y). 
div(X, Y, s(Q)) :- minus(X, Y, Z), div(Z, Y, Q).

:- prop div(Dividend, Divisor, Quotient)
#"@var{Dividend} is a natural number. \n
@var{Divisor} is a natural number. \n
@var{Quotient} is a natural number.".

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
   : (X = [[0,++], [], [+,+++]])
   => fails
   # "Cannot have blank lines.".

:- test basic_surface(X)
   : (X = [[0,1], [+++,+], [+,++]])
   => fails
   # "Line contents must have cell charge values.".

:- test basic_surface(X)
   : (X = [])
   => fails.

:- test basic_surface(X)   
   : (X = [[]])  
   => fails
   # "Cannot be a list with a empty sublist".

:- test basic_surface(X)
   : (X = [[_]])
   => not_fails
   # "Line must contain at least one cell.".

basic_surface([L]) :-
   my_list(L).

basic_surface([[H|T]]) :-  
   charge(H), 
   my_list(T).

basic_surface([L|S2]) :- 
   my_list(L), 
   basic_surface(S2).


:- doc(surface/1,"Defines a surface of charged cells, represented by a list of lists with same number of cells. 
It is defined as: @includedef{surface/1}\n").
:- prop surface(CellList) 
#"@var{CellList} is a list with charged cells.".

:- test surface(X)
   : (X = [[0,++], [], [+,+++]])
   => fails
   # "Cannot have blank lines.".

:- test surface(X)
   : (X = [[+++,1], [+++,+], [+,++]])
   => fails
   # "Line contents must have cell charge values.".

:- test surface(X)
   : X = ([])
   => fails.

:- test surface(X)
   : (X = [[]])
   => fails
   # "Cannot be a list with empty sublist.".

:- test surface(X)
   : (X = [[_]])
   => not_fails
   # "Line must contain at least one cell.".

:- test surface(X)
   : (X = [[_,_,_], [_,_], [_,_,_]])
   => fails
   # "Lines must have same length.".



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

:- doc(h_line/3,"Extracts from surface a specific horizontal line @includedef{h_line/3}\n").

:- test h_line([[0,+,++], [+++,++++,+++++]], X, [+++,++++,+++++])
   : (X = s(s(0)))
   => not_fails.

:- test h_line([[0,+,++], [+++,++++,+++++]], s(s(s(0))), _)
   => fails.

:- test h_line([[0,+,++], [+++,++++,+++++]],0, _)
   => fails.

:- test h_line([[0,+,++], [+++,++++,+++++]], s(s(0)), X)
   : (X = [+++,++++,+++++])
   => not_fails.

:- test h_line([], _, _)
   => fails.

:- test h_line([[]],_,_)
   => fails.

h_line([L], s(0), L).   
h_line(S, N, C) :-
   get(S, N, C),
   surface(S).

:- test v_line([[0,+,++], [0,+,++]], X, [+,+])
   : (X = s(s(0)))
   => not_fails.

:- test v_line([[0,+,++], [0,+,++]], s(s(s(s(0)))), _)
   => fails.

:- test v_line([[0,+,++], [0,+,++]], 0, _)
   => fails.

:- test v_line([[0,+,++], [0,+,++]], s(s(0)), X)
   : (X = [+,+])
   => not_fails.

:- test v_line([], _, _)
   => fails.

:- test v_line([[]],_,_)
   => fails.

v_line([Fila], Indice, [Elemento]) :- get(Fila, Indice, Elemento).
v_line([Fila|Filas], Indice, [Elemento|Columna]) :-
   get(Fila, Indice, Elemento),
   v_line(Filas, Indice, Columna).

v_lines([L], L).
v_lines([L|S2], C) :-
   mylength(L, Tam),
   v_lines_aux([L|S2], Tam, C). 


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
h_sum([C], N) :- f(C, N).
h_sum([H|T], Suma) :-
   h_sum(T, SumaResto),
   f(H, N),
   plus(N, SumaResto, Suma).

:- test total_charge([[0,+],[++,+++],[++++,+++++]], X)
   : X = s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))
   => not_fails.

:- test total_charge([[0,+],[++,3],[++++,+++++]], _)
   => fails.

:- test total_charge(_, X)
   : X = 15
   => fails.

:- test total_charge([[_,_],[_,_,_],[_,_]], _)
   => fails.

:- test total_charge([[]], _)
   => fails.

:-test total_charge([],_)
   => fails.

total_charge(S, T) :-
   total_charge_aux(S,T), 
   surface(S).

total_charge_aux([],0).
total_charge_aux([L|S2], T) :-
   total_charge_aux(S2, SumaResto),
   h_sum(L, SumaFila),
   plus(SumaFila, SumaResto, T).


total_cells([L], N) :- mylength(L,N).
total_cells([L|S2], Total) :-
   total_cells(S2, TotalResto),
   mylength(L, Esta),
   plus(Esta, TotalResto, Total).

average_charge([[]], _).
average_charge(S, A) :-
   total_charge(S, M),
   total_cells(S, T), 
   div(M, T, A).

%------------------------------------------------------------------------------------------------------------------------%

% FIN CODIGO
