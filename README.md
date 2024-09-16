# PROLOG
## Pure Logic Programming: Practise 1
### Introduction
The task at hand involves validating representations of statically loaded surfaces with variable load values assigned to 
discrete points. To facilitate analysis, we adopt several simplifications. Firstly, we constrain charges to predefined values 
according to the following predicate: 
```
charge( +++++++ ).
charge( ++++++ ).
charge( +++++ ).
charge( ++++ ).
charge( +++ ).
charge( ++ ).
charge( + ).
charge( 0 ).
```
Here, each charge is denoted by the number of '+' symbols, e.g., 
'++++' signifies a charge of four units. 
Furthermore, we discretize the two-dimensional surface, conceptualizing it as a grid of cells where each cell can hold one of 
the predefined charge values. An illustrative example of such a surface is: 
```
   +++    +++++++    0      +    ++++    0
 +++++++    +++      0      +    ++++    0
   +++       0    +++++++   +     0     ++++
   +++    +++++++    0    ++++    +      0
+++++++      0      +++     +    ++++    0
   +++    +++++++   +++     +     0      +
 +++++++    +++      0      +    ++++    0
```
For representation purposes, we employ a list of lists. The inner lists delineate cells horizontally, while the outer list 
groups cells by rows, with each element representing a load value. Thus, the above surface is structured as follows:
```
[ [ +++ , +++++++ , 0 , + , ++++ , 0 ],
[ +++++++ , +++ , 0 , + , ++++ , 0 ],
[ +++ , 0 , +++++++ , + , 0 , ++++ ],
[ +++ , +++++++ , 0 , ++++ , + , 0 ],
[ +++++++ , 0 , +++ , + , ++++ , 0 ],
[ +++ , +++++++ , +++ , + , 0 , + ],
[ +++++++ , +++ , 0 , + , ++++ , 0 ] ]
```
Surfaces can have an arbitrary number of cells in both dimensions. 
In order to accomplish this, we define two types of surfaces, both containing load values. We establish `basic_surface/1`
which must consist of at least one line, with each line containing at least one cell. It is defined by:
```
basic_surface([L]) :-
   my_list(L).
basic_surface([L|S2]) :- 
   my_list(L), 
   basic_surface(S2).
```
Additionally, we define `surface/1` as above but in addition must ensure that all lines possess the same number of cells. It is defined by:
```
surface([L|L2]) :- 
   basic_surface([L|L2]), 
   my_length(L, Tam), 
   surface_acc([L|L2], Tam).
```
As you can see, an auxiliary predicate has been employed, which accepts the size as a term. In each recursion step, it verifies 
the length of the horizontal lines. 
```
surface_acc([L],Tam) :- my_length(L, Tam).
surface_acc([[H|T]|T2], Acc) :- 
   my_length([H|T],NewAcc), 
   equal(Acc,NewAcc), 
   surface_acc(T2,NewAcc).
```


### Some examples of use:
1. Check if a `basic_surface/1` is correct:
```
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
2. Check if a `surface/1` is correct:
```
  ?- surface([[+++,+++++++,0,+,++++,0], 
              [+++++++,+++,0,+,++++,0], 
              [+++,0,+++++++,+,0,++++], 
              [+++,+++++++,0,++++,+,0], 
              [+++++++,0,+++,+,++++,0], 
              [+++,+++++++,+++,+,0,+], 
              [+++++++,+++,0,+,++++,0]]).

  yes
  ?- 
```

### Operations with Surfaces
We define certain operations that can be performed on surfaces.

#### h_line(S,N,C)
`C` is the `N`th horizontal line of the surface `S`.
`N` is a natural number in Peano notation.The horizontal lines are represented as lists with load values. 
To facilitate this objective, an auxiliary predicate has been used, tasked with extracting an element from a list.
 Specifically, it retrieves a list from within a list of lists.

##### Examples:
1. Lines begin from s(0) : 
```
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], 0, L).

no
?-
```
2. Extract 4th horizontal line: 
```
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
```
3. Returns position from line given:
```
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
```
4. Passing non-existing line: 
```
h_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [++,+++++,++++]).

no
?-
```

#### v_line(S,N,C)
`C` is the list of the Nth cells of all horizontal lines of the surface `S`.
`N` is a natural number in Peano notation. Similarly to the aforementioned predicate, an auxiliary predicate has been used, 
tasked with extracting an element from a list. Specifically, it retrieves a list from within a list of lists.
##### Examples:
1. Lines begin from s(0) : 
```
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], 0, C).

no
?-
```
2. Extract 2nd vertical line: 
```
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
```
3. Returns position from line given:
```
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
```
4. Passing non-existing line: 
```
v_line([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [+,+++,0], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], N, [++,0,+,+++++,++++,++,0]).

no
?-
```

#### v_lines(S, C)
`C` is the list of vertical lines of cells on the surface `S`. 
It utilizes an auxiliary predicate which, during each recursion, extracts the vertical line and concatenates it 
to the final list.
##### Examples:
1. Extract all vertical lines:
```
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
```
2. Surface given with empty horizontal line: 
```
v_lines([[+,+++,0], 
              [+,+++,0], 
              [++,0,++], 
              [], 
              [++,0,++], 
              [+,+,++], 
              [+,++,0]], C).

no
?-
```

#### total_charge(S,T)
`T` is the sum of all load values in the surface `S`. 
It employs an auxiliary predicate that utilizes another predicate to calculate the sum of all loads within a horizontal line, 
with each recursive step contributing to the total sum.

##### Examples:
1. Calculates total charge:
```
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
```
2. It does not allow `basic_surface/1`: 
```
?- total_charge([[+++,+++++++,0,+], 
              [+++++++,+++,0,+,++++], 
              [+++,0], 
              [+++,+++++++,0], 
              [+++++++,0,+++,+,++++], 
              [+++], 
              [+++++++,+++,0]], T).
no
?-
```

#### average_charge(S,A)
`A` is the average of all load valuesin the surface `S`.
This entails summing all the loads and dividing by the total number of cells. The result must be rounded by truncation, i.e. returning the natural prior.
For this purpose, the aforementioned predicate calculates the total sum of the load values, along with an auxiliary predicate that 
computes the total number of elements in the surface. Additionally, an integer division predicate is employed.
###### Examples:
1. Calculates average charge from example:
```
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
```

2. Surface given with wrong load values: 
```
?- average_charge([[3,7,0,1,4,0], 
              [7,3,0,1,4,0], 
              [3,0,7,1,0,4], 
              [3,7,0,4,1,0], 
              [7,0,3,1,4,0], 
              [3,7,3,1,0,1], 
              [7,3,0,1,4,0]], A).

no
?-
```

