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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                                                  %
%                                                   PRACTICA 1                                                                     %
%                                                                                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

my_list([H|T]) :- charge(H), my_list(T).


basic_surface([[H|T]]) :-  charge(H), my_list(T).
basic_surface([[H|T]|T2]) :- charge(H), my_list(T), basic_surface(T2).


:- test basic_surface(X)   
   : (X = [[0],[0],[0]])  
   => not_fails.