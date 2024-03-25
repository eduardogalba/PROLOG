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


:- doc(charge/1,"Defines possible constant values of my cell charges. 
It is defined as: @includedef{charge/1}\n").
charge( +++++++ ).
charge( ++++++ ).
charge( +++++ ).
charge( ++++ ).
charge( +++ ).
charge( ++ ).
charge( + ).
charge( 0 ).

:- doc(my_list/1,"Defines a list with [Head|Tail] structure and cell charges as elements. 
It is defined as: @includedef{my_list/1}\n").

my_list([H|T]) :- 
   charge(H), 
   my_list(T).

my_list([+++++++]).
my_list([++++++]).
my_list([+++++]).
my_list([++++]).
my_list([+++]).
my_list([++]).
my_list([+]).
my_list([0]).
my_list([]).



:- doc(basic_surface/1,"Defines a surface of charged cells represented by a list of lists. 
It is defined as: @includedef{basic_surface/1}\n
@begin{note}
@bf{Note:} @pred{basic_surface/1} must have at least one sublist with a charged cell.
@end{note}").
:- prop basic_surface(CellList) 
#"@var{CellList} is a list with charged cells.".
:- doc(basic_surface/1, "
   @section{Other properties}
   @subsection{Test}
   @em{It will be introduced bla bla}

").

:- test basic_surface(X)   
   : (X = [[]])  
   => fails
   # "Cannot be a list with a empty sublist".


basic_surface([[H|T]]) :-  
   charge(H), 
   my_list(T).

basic_surface([[H|T]|T2]) :- 
   charge(H), 
   my_list(T), 
   basic_surface(T2).

mylength([],0).
mylength([_|T],s(N)) :-
    mylength(T,N).


:- doc(surface/1,"Defines a surface of charged cells, represented by a list of lists with same number of cells. 
It is defined as: @includedef{surface/1}\n").
:- prop surface(CellList) 
#"@var{CellList} is a list with charged cells.".

surface([L|L2]) :- 
   basic_surface([L|L2]), 
   mylength(L, Tam), 
   surface_acc([L|L2], Tam).

surface_acc([],_).
surface_acc([[H|T]|T2], Acc) :- 
   charge(H), my_list(T), 
   mylength([H|T],NewAcc), 
   Acc == NewAcc, 
   surface_acc(T2,NewAcc).

get([Elem|_], s(0), Elem).
get([_|Rest], s(Index), Elem) :- 
   get(Rest, Index, Elem).

h_line([L|L2], N, C) :-
   surface([L|L2]),
   get([L|L2], N, C).
   


v_line([L|L2], N, C) :-
   surface([L|L2]),
   v_line_aux([L|L2], N, C).
   

myappend([],L,L) :- 
   list(L).
myappend([X|Xs],Ys,[X|Zs]) :- 
   myappend(Xs,Ys,Zs).

v_line_aux(_,_,[]).
v_line_aux([L|L2], N, [E|T]) :-
   get(L, N, E),
   v_line_aux(L2, N, T).


total_charge([L|[L2|T]], ) :- 
   myappend(L,L2, Z),
