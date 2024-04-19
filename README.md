# PROLOG
## Introduction

The task at hand involves validating representations of statically loaded surfaces with variable load values assigned to discrete points. To facilitate analysis, we adopt several simplifications. Firstly, we constrain charges to predefined values according to the following predicate:

```prolog
charge(+++++++).
charge(++++++).
charge(+++++).
charge(++++).
charge(+++).
charge(++).
charge(+).
charge(0).

Here, each charge is denoted by the number of '+' symbols, e.g., '++++' signifies a charge of four units.

Furthermore, we discretize the two-dimensional surface, conceptualizing it as a grid of cells where each cell can hold one of the predefined charge values. An illustrative example of such a surface is:

   +++    +++++++    0      +    ++++    0
 +++++++    +++      0      +    ++++    0
   +++       0    +++++++   +     0     ++++
   +++    +++++++    0    ++++    +      0
+++++++      0      +++     +    ++++    0
   +++    +++++++   +++     +     0      +
 +++++++    +++      0      +    ++++    0

For representation purposes, we employ a list of lists. The inner lists delineate cells horizontally, while the outer list groups cells by rows, with each element representing a load value. Thus, the above surface is structured as follows:
Surfaces can have an arbitrary number of cells in both dimensions.

In order to accomplish this, we define two types of surfaces, both containing load values. We establish basic_surface/1 which must consist of at least one line, with each line containing at least one cell. It is defined by:
basic_surface([L]) :-
    my_list(L).
basic_surface([L|S2]) :-
    my_list(L),
    basic_surface(S2).
Additionally, we define surface/1 as above but in addition must ensure that all lines possess the same number of cells. It is defined by:
surface([L|L2]) :-
    basic_surface([L|L2]),
    mylength(L,Tam),
    surface_acc([L|L2],Tam).
surface_acc([L],Tam) :-
    mylength(L,Tam).
surface_acc([[H|T]|T2],Acc) :-
    mylength([H|T],NewAcc),
    equal(Acc,NewAcc),
    surface_acc(T2,NewAcc).

