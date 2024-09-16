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
## ISO PROLOG Programming: Practise 2

## Introduction
On the state, daily watering of trees is essential. To automate this task, the state owner has
procured a robotic device capable of autonomously watering trees and preparing soil for new
plantings. This robotic system initiates operation at dawn, tasked with watering each tree
individually. Each tree is identified by a constant Prolog term and requires a specific 
amount of water, denoted by a binary predicate, indicating if tree T needs C units of water
daily. @p
@begin{verbatim}
💧🌳                      🌳💧
 2 1 ________18____________ 2 1
   |\\                    / |
   | \\                  /  |
   |  13               19  |
   |   \\____      ____/    |
  19     ___ ⛲🤖____     15
   |    /             \\    |
   |  22               \\   |
   |  /                 34 |
   | /                   \\ |
💧🌳                      🌳💧
 2 3 _________8___________ 4 4
@end{verbatim}
The watering procedure is as follows: every morning, the robot positions itself by the well.
Upon activation, it fills its initially empty bucket with water from the well, then proceeds 
to water each tree sequentially. If the bucket empties before all trees are watered, the 
robot returns to the well, refills the bucket, and resumes watering. Once all trees are 
watered or the bucket empties again, the robot refills the bucket if necessary and continues 
the task. Upon completion, it notifies the state owner telematically.@p
Additionally, the robot must adhere to the following rules: @p
- It must never attempt to water a tree without sufficient water in the bucket, constrained 
by the bucket's capacity (determined by the predicate).
- Paths connecting trees and the well vary in length, each taking a specific time to 
traverse. These paths are represented by ternary and binary predicates, ensuring efficient 
movement.
- Due to potential obstacles (e.g., ponds, buildings, fences), not all points on the 
state may be connected. Consequently, the robot may encounter inaccessible areas.
- The robot can return to the well only when the bucket is empty, except after 
completing all watering cycles.
- Bucket refilling at the well always takes a fixed amount of time.
- Each tree must be visited and watered exactly once during the task execution.
- All distances, time units, and water quantities involved are represented as integers.
Moreover, the robot, simulating artificial intelligence, anticipates future tasks such as 
digging holes for new tree planting, prompting it to prolong watering tasks while adhering 
to the aforementioned rules. @p
It's essential to note that state descriptions are solely based on predicate representations, 
ensuring consistency and compatibility with the implemented predicates.
### Some examples of use:
1. Enum possible trees on the state:
```
:- module(_,_,[classic ,assertions ,regtypes]).
arbol(arbol1).
arbol(arbol2).
arbol(arbol3).
arbol(arbol4).
camino_arbol_arbol( arbol1 , arbol2 , 18 ).
camino_arbol_arbol( arbol2 , arbol3 , 12 ).
camino_arbol_arbol( arbol1 , arbol3 , 19 ).
camino_arbol_arbol( arbol4 , arbol3 , 8 ).
camino_arbol_arbol( arbol4 , arbol2 , 20 ).
camino_arbol_pozo( arbol1 , 13 ).
camino_arbol_pozo( arbol2 , 19 ).
camino_arbol_pozo( arbol3 , 22 ).
camino_arbol_pozo( arbol4 , 34 ).
necesita( arbol1 , 2 ).
necesita( arbol2 , 1 ).
necesita( arbol3 , 2 ).
necesita( arbol4 , 4 ).
```
```
?- arbol(T).
``` 
2. Enum possible paths between trees:
```ciao_runnable
?- camino_arbol_arbol(T1, T2, D).
``` 
3. Enum possible paths between trees and well:
```ciao_runnable
?- camino_arbol_pozo(T, D).
``` 
4. Enum water units needed by trees:
```ciao_runnable
?- necesita( T, W).
``` 

## Movement and Watering on the state
We define certain changes of state which indicates robot movements and watering order.

### From well to a tree
de_pozo_a_regar_arbol(A, D, NV, ND) @p
Represents the change of state if the robot is initially located in the well, 
the robot goes directly from there to the tree `A` and waters it.
#### Examples:
1. Let´s water tree 1 at the beginning: 
```
?- necesita(arbol1, V).
```
```
?- camino_arbol_pozo(arbol1, D).
```
```
?- de_pozo_a_regar_arbol(arbol1, 0, NV, ND).

ND = 13,
NV = 3 ? 

yes
?-
```
2. Let´s water tree 5 at the beginning: 
```
?- arbol(arbol5).
```
```
?- de_pozo_a_regar_arbol(arbol5, 0, NV, ND).

no
?-
````

### From a tree to another tree
regar_otro_arbol(A, NA, V, NV, D, ND) @p
Represents the change of state if the robot is initially located in tree `A` and the robot 
goes directly from there to tree `NA` and waters it.
#### Examples:
1. Let´s water tree 2 from tree 1 (at 20 time units with enough water units): 
```
?- camino_arbol_arbol(arbol1, arbol2, D).
```
```
?- necesita(arbol2, V).
```
```
?- regar_otro_arbol(arbol1, arbol2,5,NV,20,ND).

ND = 38,
NV = 4 ? 

yes
?-
```
2. Let´s water tree 1 from tree 2 (There´s no path clause):
```
?- camino_arbol_arbol(arbol2, arbol1, D).
```
```
?- necesita(arbol1, V).
```
```
?- regar_otro_arbol(arbol2, arbol1,5,NV,20,ND).

ND = 38,
NV = 4 ? 

yes
?-
```
3. Let´s water with not enough water units:
```
?- necesita(arbol4, V).
```
```
?- regar_otro_arbol(arbol2,arbol4,3,NV,20,ND).

no
?-
```

### Movement from the well
movimiento_desde_pozo(T, DA, DT) @p
Will be possible if `DT` is the number of time units elapsed from the start of watering 
until the robot has watered all trees in sequence `T` and has returned to the well.
This predicate can be called both at the beginning and at an intermediate point  when the 
bucket needs to be refilled, i.e., after the bucket has been refilled.
#### Examples:
1. From a tree to another:
```
?- movimiento_desde_pozo([arbol2,arbol1], 20, DT).

DT = 70 ? 

yes
?-
```
2. From a tree to another (no possible move):
```
?- movimiento_desde_pozo([arbol1,arbol4], 20, DT).

no
?-
```
3. From a tree to others:
```
?- movimiento_desde_pozo([arbol3,arbol1,arbol2,arbol4], 0, DT).

DT = 146 ? 

yes
?-
```

### Movement from a tree to another
movimiento_desde_arbol(T, A, V, DA, DT) @p
Will be possible if `DT` is the number of time units elapsed from the start of watering 
until the robot has watered all the trees in the sequence `T`. Sometimes there will not 
be enough water available to water the first tree in sequence T . In this case, this predicate 
must not fail, so we must return to the well.

#### Examples:
1. From a tree to another:
```
?- movimiento_desde_arbol([arbol1], arbol2, 10, 20, DT).

DT = 57 ? 

yes
?-
```
2. From a tree to another (reverse path):
```
?- movimiento_desde_arbol([arbol2], arbol1, 10, 20, DT).

DT = 57 ? 

yes
?-
```
3. From a tree to another (no possible move):
```
?- movimiento_desde_arbol([arbol4], arbol1, 10, 20, DT).

no
?-
```
4. From a tree to others:
```
?- movimiento_desde_arbol([arbol1,arbol3,arbol4], arbol2, 8, 20, DT).

DT = 99 ? 

yes
?-
```
5. From a tree to others (no possible move):
```
?- movimiento_desde_arbol([arbol1,arbol3,arbol4], arbol2, 7, 20, DT).

no
?-
```

### Trajectories
trayectoria_valida(A,D,T) @p
Will be possible if `T` is a valid watering trajectory of duration `D` time units, 
created from the trees belonging to list `A` . A watering trajectory is defined as a sequence 
of tree identifiers indicating the order in which the robot has watered the trees in it. @p
A watering trajectory is valid if and only if 
- It contains exactly once each and every tree on the state. 
- It can be executed by the robot following the rules given in the statement.
#### Examples:

1. Valid trajectory between all the trees:
```
?- trayectoria_valida([arbol1,arbol2,arbol3,arbol4], D, T).

D = 131,
T = [arbol1,arbol3,arbol2,arbol4] ? ;

D = 146,
T = [arbol2,arbol1,arbol3,arbol4] ? ;

D = 131,
T = [arbol2,arbol3,arbol1,arbol4] ? ;

D = 127,
T = [arbol2,arbol4,arbol1,arbol3] ? ;

D = 127,
T = [arbol2,arbol4,arbol3,arbol1] ? ;

...
```

2. Two trees with no trajectory:
```
?- trayectoria_valida([arbol1,arbol4], D, T).

no
?-
```

### Optimal watering trajectories
riego(D,T) @p
Will be possible if `T` is a valid watering trajectory that takes D units of time and is 
optimal, in order to maximising the duration of watering. This means, there is no other valid 
trajectory whose duration is shorter than `D`.
    
#### Examples:

1. Optimal watering trajectories on this state:
```
?- riego(T,D).

D = 146,
T = [arbol2,arbol1,arbol3,arbol4] ? ;

D = 146,
T = [arbol3,arbol1,arbol2,arbol4] ? ;

no
?-
```
