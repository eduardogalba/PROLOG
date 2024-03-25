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


charge( +++++++ ).
charge( ++++++ ).
charge( +++++ ).
charge( ++++ ).
charge( +++ ).
charge( ++ ).
charge( + ).
charge( 0 ).



my_list([+++++++]).
my_list([++++++]).
my_list([+++++]).
my_list([++++]).
my_list([+++]).
my_list([++]).
my_list([+]).
my_list([0]).
my_list([]).

my_list([H|T]) :- 
   charge(H), 
   my_list(T).

:- doc(basic_surface/1,"Defines a surface of charged cells and it´s represented by a list of lists. It is defined as: @includedef{basic_surface/1}\n
@begin{alert}
@bf{Note:} @tt{basic_surface/1} must have at least one sublist with a charged cell.
@end{alert}").
:- prop basic_surface(CellList) 
#"@var{CellList} is a list with charged cells.".


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

