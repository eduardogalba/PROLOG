%:- module(_,_,[assertions,regtypes]).
% :- module(_,_,[]).           % For pure LP, depth-first search rule
:- module(_,_,['sr/bfall']).   % For pure LP, breadth-first search rule, all predicates

author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

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

my_list([]).
my_list([_|T]) :- my_list(T).

basic_surface([S]) :- my_list(S).
basic_surface([[_|T]|T2]) :- my_list(T),basic_surface(T2). 