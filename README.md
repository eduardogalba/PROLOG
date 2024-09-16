# PROLOG
## Pure Logic Programming: Practise 1
### Introduction
The task at hand involves validating representations of statically loaded surfaces with variable load values assigned to 
discrete points. To facilitate analysis, we adopt several simplifications. Firstly, we constrain charges to predefined values 
according to the following predicate: @includedef{charge/1} Here, each charge is denoted by the number of '+' symbols, e.g., 
'++++' signifies a charge of four units. @p
Furthermore, we discretize the two-dimensional surface, conceptualizing it as a grid of cells where each cell can hold one of 
the predefined charge values. An illustrative example of such a surface is: @p
``
   +++    +++++++    0      +    ++++    0
 +++++++    +++      0      +    ++++    0
   +++       0    +++++++   +     0     ++++
   +++    +++++++    0    ++++    +      0
+++++++      0      +++     +    ++++    0
   +++    +++++++   +++     +     0      +
 +++++++    +++      0      +    ++++    0
``
For representation purposes, we employ a list of lists. The inner lists delineate cells horizontally, while the outer list 
groups cells by rows, with each element representing a load value. Thus, the above surface is structured as follows:@p
``
[ [ +++ , +++++++ , 0 , + , ++++ , 0 ],
[ +++++++ , +++ , 0 , + , ++++ , 0 ],
[ +++ , 0 , +++++++ , + , 0 , ++++ ],
[ +++ , +++++++ , 0 , ++++ , + , 0 ],
[ +++++++ , 0 , +++ , + , ++++ , 0 ],
[ +++ , +++++++ , +++ , + , 0 , + ],
[ +++++++ , +++ , 0 , + , ++++ , 0 ] ]
``
Surfaces can have an arbitrary number of cells in both dimensions. @p
In order to accomplish this, we define two types of surfaces, both containing load values. We establish @prop{basic_surface/1}
which must consist of at least one line, with each line containing at least one cell. It is defined by:@p
@includedef{basic_surface/1} 
Additionally, we define @prop{surface/1} as above but in addition must ensure that all lines possess the same number of cells. It is defined by:@p
@includedef{surface/1}
As you can see, an auxiliary predicate has been employed, which accepts the size as a term. In each recursion step, it verifies 
the length of the horizontal lines. @p
@includedef{surface_acc/2}


### Some examples of use:
@begin{enumerate}
@item Check if a @prop{basic_surface/1} is correct:
``
```ciao
?- basic_surface([[+++,+++++++,0,+], 
              [+++++++,+++,0,+,++++], 
              [+++,0], 
              [+++,+++++++,0], 
              [+++++++,0,+++,+,++++], 
              [+++], 
              [+++++++,+++,0]]).

  yes
?-
``` 
``
@item Check if a @prop{surface/1} is correct: @p
``
  ?- surface([[+++,+++++++,0,+,++++,0], 
              [+++++++,+++,0,+,++++,0], 
              [+++,0,+++++++,+,0,++++], 
              [+++,+++++++,0,++++,+,0], 
              [+++++++,0,+++,+,++++,0], 
              [+++,+++++++,+++,+,0,+], 
              [+++++++,+++,0,+,++++,0]]).

  yes
  ?- 
``
@end{enumerate}

@section{Operations with surfaces}
We define certain operations that can be performed on surfaces.

### h_line(S,N,C)
@var{C} is the @var{N}th horizontal line of the surface @var{S} @p
@var{N} is a natural number in Peano notation.The horizontal lines are represented as lists with load values. 
To facilitate this objective, an auxiliary predicate has been used, tasked with extracting an element from a list.
 Specifically, it retrieves a list from within a list of lists.

@subsubsection{Examples}
@begin{enumerate}
@item Lines begin from s(0) : 
``
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], 0, L).

no
?-
``
@item Extract 4th horizontal line: @p 
``
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
``
@item Returns position from line given:
``
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
``
@item Passing non-existing line: @p
``
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [++,+++++,++++]).

no
?-
``
@end{enumerate}

### v_line(S,N,C)
@var{C} is the list of the Nth cells of all horizontal lines of the surface @var{S}. @p
@var{N} is a natural number in Peano notation. Similarly to the aforementioned predicate, an auxiliary predicate has been used, 
tasked with extracting an element from a list. Specifically, it retrieves a list from within a list of lists.
@subsubsection{Examples}
@begin{enumerate}
@item Lines begin from s(0) : 
``
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], 0, C).

no
?-
``
@item Extract 2nd vertical line: @p
``
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
``
@item Returns position from line given:
``
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
``
@item Passing non-existing line: @p
``
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [++,0,+,+++++,++++,++,0]).

no
?-
``
@end{enumerate}

### v_lines(S, C)
@var{C} is the list of vertical lines of cells on the surface @var{S}. @p
It utilizes an auxiliary predicate which, during each recursion, extracts the vertical line and concatenates it 
to the final list.
@subsubsection{Examples}
@begin{enumerate}
@item Extract all vertical lines:
``
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
``
@item Surface given with empty horizontal line: @p
``
v_lines([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], C).

no
?-
``
@end{enumerate}

### total_charge(S,T)
@var{T} is the sum of all load values in the surface @var{S}. @p
It employs an auxiliary predicate that utilizes another predicate to calculate the sum of all loads within a horizontal line, 
with each recursive step contributing to the total sum.

@subsubsection{Examples}
@begin{enumerate}
@item Calculates total charge:
``
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
``
@item It does not allow @prop{basic_surface/1}: @p
``
?- total_charge([[+++,+++++++,0,+], 
              [+++++++,+++,0,+,++++], 
              [+++,0], 
              [+++,+++++++,0], 
              [+++++++,0,+++,+,++++], 
              [+++], 
              [+++++++,+++,0]], T).
no
?-
``
@end{enumerate}

### average_charge(S,A)
@var{A} is the average of all load valuesin the surface @var{S}.@p
This entails summing all the loads and dividing by the total number of cells. The result must be rounded by truncation, i.e. returning the natural prior.
For this purpose, the aforementioned predicate calculates the total sum of the load values, along with an auxiliary predicate that 
computes the total number of elements in the surface. Additionally, an integer division predicate is employed.
@subsubsection{Examples}
@begin{enumerate}

@item Calculates average charge from example:
``
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
``

@item Surface given with wrong load values: @p
``
?- average_charge([[3,7,0,1,4,0], 
              [7,3,0,1,4,0], 
              [3,0,7,1,0,4], 
              [3,7,0,4,1,0], 
              [7,0,3,1,4,0], 
              [3,7,3,1,0,1], 
              [7,3,0,1,4,0]], A).

no
?-
``
@end{enumerate}

