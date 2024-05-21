:- module(_,_,[classic ,assertions ,regtypes]).

:- prop camino_arbol_arbol(T1,T2, D) :: (arbol(T1), arbol(T2), number(D)).

:- dynamic(camino_arbol_arbol/3). % Distancias entre arboles
camino_arbol_arbol( arbol1 , arbol2 , 18 ).
camino_arbol_arbol( arbol2 , arbol3 , 12 ).
camino_arbol_arbol( arbol1 , arbol3 , 19 ).
camino_arbol_arbol( arbol4 , arbol3 , 8 ).
camino_arbol_arbol( arbol4 , arbol2 , 20 ).
:- dynamic(camino_arbol_pozo/2).
% Distancias entre el pozo y los arboles.
:- prop camino_arbol_pozo(T, D) :: (arbol(T), number(D)).

camino_arbol_pozo( arbol1 , 13 ).
camino_arbol_pozo( arbol2 , 19 ).
camino_arbol_pozo( arbol3 , 22 ).
camino_arbol_pozo( arbol4 , 34 ).
:- dynamic(necesita/2).
% Cantidad de agua necesaria para regar cada arbol.
:- pred necesita(T, D) 
    :: (arbol(T), number(D))
    #"Defines the water units that a tree needs. It is defined by: 
    @includedef{necesita/2}".

necesita( arbol1 , 2 ).
necesita( arbol2 , 1 ).
necesita( arbol3 , 2 ).
necesita( arbol4 , 4 ).
:- dynamic(capacidad/1).
% Capacidad del cubo.
:- prop capacidad(C) :: (number(C)).
capacidad(5).

%----------------------------------------------------------------------------------%
:- doc(author_data/4,"Defines authors in Deliverit system.").

author_data('Gil', 'Alba', 'Eduardo', 'Z170238').

:- doc(title, "ISO Prolog Programming: Practise 2").
:- doc(author, "Eduardo Gil Alba").
:- doc(module, "
@section{Introduction}
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
@begin{enumerate}
@item It must never attempt to water a tree without sufficient water in the bucket, constrained 
by the bucket's capacity (determined by the predicate).
@item Paths connecting trees and the well vary in length, each taking a specific time to 
traverse. These paths are represented by ternary and binary predicates, ensuring efficient 
movement.
@item Due to potential obstacles (e.g., ponds, buildings, fences), not all points on the 
state may be connected. Consequently, the robot may encounter inaccessible areas.
@item The robot can return to the well only when the bucket is empty, except after 
completing all watering cycles.
@item Bucket refilling at the well always takes a fixed amount of time.
@item Each tree must be visited and watered exactly once during the task execution.
@item All distances, time units, and water quantities involved are represented as integers.
@end{enumerate}
Moreover, the robot, simulating artificial intelligence, anticipates future tasks such as 
digging holes for new tree planting, prompting it to prolong watering tasks while adhering 
to the aforementioned rules. @p
It's essential to note that state descriptions are solely based on predicate representations, 
ensuring consistency and compatibility with the implemented predicates.
@subsection{Some examples of use:}
@begin{enumerate}
@item Enum possible trees on the state:
```ciao_runnable
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
```ciao_runnable
?- arbol(T).
``` 
@item Enum possible paths between trees:
```ciao_runnable
?- camino_arbol_arbol(T1, T2, D).
``` 
@item Enum possible paths between trees and well:
```ciao_runnable
?- camino_arbol_pozo(T, D).
``` 
@item Enum water units needed by trees:
```ciao_runnable
?- necesita( T, W).
``` 
@end{enumerate}

@section{Movement and Watering on the state}
We define certain changes of state which indicates robot movements and watering order.

@subsection{From well to a tree}
de_pozo_a_regar_arbol(A, D, NV, ND) @p
Represents the change of state if the robot is initially located in the well, 
the robot goes directly from there to the tree @var{A} and waters it.
@subsubsection{Examples}
@begin{enumerate}
@item Let´s water tree 1 at the beginning: 
```ciao_runnable
?- necesita(arbol1, V).
```
```ciao_runnable
?- camino_arbol_pozo(arbol1, D).
```
```ciao
?- de_pozo_a_regar_arbol(arbol1, 0, NV, ND).

ND = 13,
NV = 3 ? 

yes
?-
```
@item Let´s water tree 5 at the beginning: 
```ciao_runnable
?- arbol(arbol5).
```
```ciao
?- de_pozo_a_regar_arbol(arbol5, 0, NV, ND).

no
?-
````
@end{enumerate}

@subsection{From a tree to another tree}
regar_otro_arbol(A, NA, V, NV, D, ND) @p
Represents the change of state if the robot is initially located in tree @var{A} and the robot 
goes directly from there to tree @var{NA} and waters it.
@subsubsection{Examples}
@begin{enumerate}
@item Let´s water tree 2 from tree 1 (at 20 time units with enough water units): 
```ciao_runnable
?- camino_arbol_arbol(arbol1, arbol2, D).
```
```ciao_runnable
?- necesita(arbol2, V).
```
```ciao
?- regar_otro_arbol(arbol1, arbol2,5,NV,20,ND).

ND = 38,
NV = 4 ? 

yes
?-
```
@item Let´s water tree 1 from tree 2 (There´s no path clause):
```ciao_runnable
?- camino_arbol_arbol(arbol2, arbol1, D).
```
```ciao_runnable
?- necesita(arbol1, V).
```
```ciao
?- regar_otro_arbol(arbol2, arbol1,5,NV,20,ND).

ND = 38,
NV = 4 ? 

yes
?-
```
@item Let´s water with not enough water units:
```ciao_runnable
?- necesita(arbol4, V).
```
```ciao
?- regar_otro_arbol(arbol2,arbol4,3,NV,20,ND).

no
?-
```
@end{enumerate}

@subsection{Movement from the well}
movimiento_desde_pozo(T, DA, DT) @p
Will be possible if @var{DT} is the number of time units elapsed from the start of watering 
until the robot has watered all trees in sequence @var{T} and has returned to the well.
This predicate can be called both at the beginning and at an intermediate point  when the 
bucket needs to be refilled, i.e., after the bucket has been refilled.
@subsubsection{Examples}
@begin{enumerate}
@item From a tree to another:
```ciao
?- movimiento_desde_pozo([arbol2,arbol1], 20, DT).

DT = 70 ? 

yes
?-
```
@item From a tree to another (no possible move):
```ciao
?- movimiento_desde_pozo([arbol1,arbol4], 20, DT).

no
?-
```
@item From a tree to others:
```ciao
?- movimiento_desde_pozo([arbol3,arbol1,arbol2,arbol4], 0, DT).

DT = 146 ? 

yes
?-
```
@end{enumerate}

@subsection{Movement from a tree to another}
movimiento_desde_arbol(T, A, V, DA, DT) @p
Will be possible if @var{DT} is the number of time units elapsed from the start of watering 
until the robot has watered all the trees in the sequence @var{T}. Sometimes there will not 
be enough water available to water the first tree in sequence T . In this case, this predicate 
must not fail, so we must return to the well.

@subsubsection{Examples}
@begin{enumerate}
@item From a tree to another:
```ciao
?- movimiento_desde_arbol([arbol1], arbol2, 10, 20, DT).

DT = 57 ? 

yes
?-
```
@item From a tree to another (reverse path):
```ciao
?- movimiento_desde_arbol([arbol2], arbol1, 10, 20, DT).

DT = 57 ? 

yes
?-
```
@item From a tree to another (no possible move):
```ciao
?- movimiento_desde_arbol([arbol4], arbol1, 10, 20, DT).

no
?-
```
@item From a tree to others:
```ciao
?- movimiento_desde_arbol([arbol1,arbol3,arbol4], arbol2, 8, 20, DT).

DT = 99 ? 

yes
?-
```
@item From a tree to others (no possible move):
```ciao
?- movimiento_desde_arbol([arbol1,arbol3,arbol4], arbol2, 7, 20, DT).

no
?-
```
@end{enumerate}

@subsection{Trajectories}
trayectoria_valida(A,D,T) @p
Will be possible if @var{T} is a valid watering trajectory of duration @var{D} time units, 
created from the trees belonging to list @var{A} . A watering trajectory is defined as a sequence 
of tree identifiers indicating the order in which the robot has watered the trees in it. @p
A watering trajectory is valid if and only if 
- It contains exactly once each and every tree on the state. 
- It can be executed by the robot following the rules given in the statement.
@subsubsection{Examples}
@begin{enumerate}

@item Valid trajectory between all the trees:
```ciao
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

@item Two trees with no trajectory:
```ciao
?- trayectoria_valida([arbol1,arbol4], D, T).

no
?-
```
@end{enumerate}
@subsection{Optimal watering trajectories}
riego(D,T) @p
Will be possible if @var{T} is a valid watering trajectory that takes D units of time and is 
optimal, in order to maximising the duration of watering. This means, there is no other valid 
trajectory whose duration is shorter than @var{D}.
    
@subsubsection{Examples}
@begin{enumerate}

@item Optimal watering trajectories on this state:
```ciao
?- riego(T,D).

D = 146,
T = [arbol2,arbol1,arbol3,arbol4] ? ;

D = 146,
T = [arbol3,arbol1,arbol2,arbol4] ? ;

no
?-
```
@end{enumerate}").

%---------------------------------------------------------------------------------%
% OPERACIONES CON LISTAS

:-pred is_set(L) 
    :: (list(L))
    #"Checks there is not duplicates in a list. It is defined by:
    @includedef{is_set/1}".

is_set([]).
is_set([H|T]) :-
    \+ member(H, T),  
    is_set(T). 

:-pred permute(L, P) 
    :: (list(L), list(P))
    #"Generate permutations from a list. It is defined by:
    @includedef{permute/2}".

permute(LT, T) :-
    ground(LT),var(T),
    permute_(LT, T).
permute(LT, T) :-
    ground(T),
    qsort(T, LT). 

:-pred permute_(L, P) 
    :: (list(L), list(P))
    #"Auxiliary predicate which helps predicate @pred{permute/2}. It is defined by:
    @includedef{permute_/2}".

permute_([],[]).
permute_(LT, [H|P]) :-
    select(H,LT,T),
    permute_(T, P).

:- pred qsort(S, SL) 
    :: (list(S), list(SL))
    #"Sort a list lexicographically. It is defined by:
    @includedef{qsort/2}".

qsort(L,SL) :-
    qsort_(L,SL,[]).

:- pred qsort_(L, SL, SLE) 
    :: (list(L), list(SL), list(SLE))
    #"Auxiliary predicate which helps predicate @pred{qsort/2}. It is defined by:
    @includedef{qsort_/3}".

qsort_([],SLE,SLE).
qsort_([X|L],SL,SLE) :-
    partition(L,X,S,B),
    qsort_(S,SL,[X|BS]),
    qsort_(B,BS,SLE).

:- pred partition(L, E, Smalls, Bigs)
    :: (list(L), number(E), list(Smalls), list(Bigs))
    #"Splits a list in two lists sorted by an element. It is defined by:
    @includedef{partition/4}". 

partition([],_P,[],[]).
partition([E|R],P,[E|Smalls],Bigs) :- % Take first element E
    E @< P, % If E < P add to list of smaller ones
    partition(R,P,Smalls ,Bigs).
partition([E|R],P,Smalls ,[E|Bigs]) :-
    E @> P, % If E >= P add to list of larger ones
    partition(R,P,Smalls ,Bigs).

:- pred max_list(L, E) 
    :: (list(L), number(E))
    #"Extracts the maximum element of a list. It is defined by:
    @includedef{max_list/2}".

max_list( [H], H).
max_list([H,K|T],M) :- H >= K, !, max_list([H|T],M). 
max_list([H,K|T],M) :- H < K,  max_list([K|T],M).
%---------------------------------------------------------------------------------%

:- prop arbol(T)
    #"@var{T} is a tree.".

arbol(arbol1).
arbol(arbol2).
arbol(arbol3).
arbol(arbol4).

:- test (arbol(A))
    :(A = arbol1) + not_fails
    #"Test 1.1: Arbol existente en la finca(1)".

:- test (arbol(A))
    :(A =arbol2) + not_fails
    #"Test 1.2: Arbol existente en la finca(2)".

:- test (arbol(A))
    :(A = arbol3) + not_fails
    #"Test 1.3: Arbol existente en la finca(3)".

:- test (arbol(A))
    :(A = arbol4) + not_fails
    #"Test 1.4: Arbol existente en la finca(4)".

:- test (arbol(A))
    :(A = arbol5) + fails
    #"Test 1.5: No existe ese arbol en la finca (1)".

:- test (arbol(A))
    :(A = arbol6) + fails
    #"Test 1.6: No existe ese arbol en la finca (2)".



:- prop lista_de_arboles(LT) 
    #"@var{LT} is a list of trees on the state".

lista_de_arboles(LT) :-
    setof(X,arbol(X),LT).

:- test (lista_de_arboles(LT))
    : (LT = [arbol1, arbol2, arbol3, arbol4]) + not_fails
    #"Test 2.1: Lista ordenada correcta".

:- test (lista_de_arboles(LT))
    : (LT = [arbol2, arbol1, arbol3, arbol4]) + fails
    #"Test 2.2: La lista no esta ordenada(1)".

:- test (lista_de_arboles(LT))
    : (LT = [arbol3, arbol2, arbol1, arbol4]) + fails
    #"Test 2.3: La lista no esta ordenada(2)".

:- test (lista_de_arboles(LT))
    : (LT = [arbol1, arbol2, arbol2, arbol4]) + fails
    #"Test 2.4: La lista tiene duplicados(1)".

:- test (lista_de_arboles(LT))
    : (LT = [arbol1, arbol2, arbol3, arbol2, arbol4]) + fails
    #"Test 2.5: La lista tiene duplicados(2)".

:- test (lista_de_arboles(LT))
    : (LT = [arbol1, arbol2, arbol3, arbol5]) + fails
    #"Test 2.6: La lista contiene arboles que no existen(1)".

:- test (lista_de_arboles(LT))
    : (LT = [arbol1, arbol2, arbol3, arbol4, arbol5]) + fails
    #"Test 2.7: La lista contiene arboles que no existen(2)".

:- pred de_pozo_a_regar_arbol(A,DA,NV,ND) 
    :: (arbol(A), number(DA), number(NV), number(ND))
    #"Represents the change of state if the robot is initially located in the well, 
    the robot goes directly from there to the tree @var{A} and waters it. It is defined by:
    @includedef{de_pozo_a_regar_arbol/4}".

/* Asumo que si el robot esta en el pozo, el cubo esta lleno de agua, se comprueba la cantidad
que necesita el arbol, pero en principio todos los arboles deberian de necesitar menos de 
la capacidad o nunca seran regados */

de_pozo_a_regar_arbol(A,DA,NV,ND) :-
    arbol(A), 
    capacidad(C),
    necesita(A, V),
    NV is C - V,
    camino_arbol_pozo(A, D),
    ND is DA + D.

:- test (de_pozo_a_regar_arbol(A, DA, NV, ND)) 
    : (A = arbol1, DA = 0)
    => (NV = 3, ND = 13)
    #"Test 3.1: Devuelveme cuanto queda en el cubo y que tiempo ha transcurrido".

:- test (de_pozo_a_regar_arbol(A, DA, NV, ND))
    : (DA = 0, ND = 22)
    => (A = arbol3, NV = 3)
    #"Test 3.2: Arbol visitado si ha transcurrido este tiempo(1)".

:- test (de_pozo_a_regar_arbol(A, DA, NV, ND))
    : (DA = 0, ND = 19)
    => (A = arbol2, NV = 4)
    #"Test 3.3: Arbol visitado si ha transcurrido este tiempo(2)".

:- test (de_pozo_a_regar_arbol(A, DA, NV, _))
    : (DA = 0, NV = 3)
    => (A = arbol1 ; A = arbol3)
    #"Test 3.4:Arbol visitado si esto me queda en el cubo(1)".

:- test (de_pozo_a_regar_arbol(A, DA, NV, _))
    : (DA = 0, NV = 1)
    => (A = arbol4)
    #"Test 3.5: Arbol visitado si esto me queda en el cubo(2)".

:- test (de_pozo_a_regar_arbol(A, _, _, _))
    : (A = arbol5) + fails
    #"Test 3.6: Ve a un arbol que no existe".



:- pred regar_otro_arbol(A,NA,V,NV,D,ND) 
    :: (arbol(A),arbol(NA),number(V),number(NV),number(D),number(ND))
    #"Represents the change of state if the robot is initially located in tree @var{A} and the robot 
    goes directly from there to tree @var{NA} and waters it. It is defined by:
    @includedef{regar_otro_arbol/6}".

regar_otro_arbol(A,NA,V,NV,D,ND) :-
    arbol(A),
    arbol(NA),
    necesita(NA, N),
    V >= N,
    NV is V - N,
    camino_arbol_arbol(A, NA, T),
    ND is D + T.

regar_otro_arbol(A,NA,V,NV,D,ND) :-
    arbol(A),
    arbol(NA),
    necesita(NA, N),
    V >= N,
    NV is V - N,
    camino_arbol_arbol(NA, A, T), 
    ND is D + T.

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol1, NA = arbol2, V = 4, D = 20)
    => (ND = 38, NV = 3) + not_fails
    #"Test 4.1: De un arbol a otro".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol2, NA = arbol1, V = 4, D = 20)
    => (ND = 38, NV = 2) + not_fails
    #"Test 4.2: De un arbol a otro (camino inverso)".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol2, V = 4, D = 20, ND = 32)
    => (NA = arbol3, NV = 2) + not_fails
    #"Test 4.3: Arbol visitado si ha transcurrido este tiempo(1)".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol4, V = 5, D = 20, ND = 40)
    => (NA = arbol2, NV = 4) + not_fails
    #"Test 4.4: Arbol visitado si ha transcurrido este tiempo(2)".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol5) + fails
    #"Test 4.5: De un arbol a otro que no existe".

:- test (regar_otro_arbol(A,NA,V,NV,D,ND))
    : (A = arbol2, NA = arbol1, V = 1, D = 20) + fails
    #"Test 4.6: No te queda suficiente agua".


% [3,1,2,4] 0
% P   ->  3   ->  1   ->  2   ->  P   ->  4    ->  P
% (0) -> (22) -> (41) -> (59) -> (78) -> (112) -> (146)
% (5) -> (3)  -> (1)  -> (0)  -> (5)  -> (1)   -> (5)
% P -> 1 -> 2 -> P
% [1,2,3,4]
%  P -> 1 -> 2 -> 3 -> 4 -> P

:- pred (movimiento_desde_pozo(A, DA, DT)) 
    :: (lista_de_arboles(A), number(DA), number(DT))
    #"It is true if and only if @var{DT} is the number of time units elapsed from the start of watering 
    until the robot has watered all trees in sequence @var{T} and has returned to the well. It is defined by:
    @includedef{movimiento_desde_pozo/3}".

movimiento_desde_pozo([H], DA, DT) :-
    de_pozo_a_regar_arbol(H, DA, _, DT).

movimiento_desde_pozo([H|[K|T]], DA, DT) :-
    is_set([H|[K|T]]),
    capacidad(C),
    camino_arbol_arbol(H,K,_),
    necesita(H, V),
    NV is C - V,
    movimiento_desde_pozo([H], 0, NT),
    NA is DA + NT,
    movimiento_desde_arbol([K|T], H, NV, NA, DT).

movimiento_desde_pozo([H|[K|T]], DA, DT) :-
    is_set([H|[K|T]]),
    capacidad(C),
    camino_arbol_arbol(K,H,_),
    necesita(H, V),
    NV is C - V,
    movimiento_desde_pozo([H], 0, NT),
    NA is DA + NT,
    movimiento_desde_arbol([K|T], H, NV, NA, DT).

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1], DA = 20) + not_fails
    #"Test 5.1: De un árbol a otro".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol1,arbol2], DA = 20) + not_fails
    #"Test 5.2: De un árbol a otro (camino inverso)".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol4], DA = 20) + not_fails
    #"Test 5.3: De un árbol a otro".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol1,arbol4], DA = 20) + fails
    #"Test 5.4: De un árbol a otro, movimiento no posible".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1,arbol3], DA = 20) + not_fails
    #"Test 5.5: De un árbol a otros dos".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1,arbol3,arbol4], DA = 20) + not_fails
    #"Test 5.6: De un árbol a los otros tres".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol1,arbol4,arbol3], DA = 20) + fails
    #"Test 5.7: Movimiento no posible".

:- test (movimiento_desde_pozo(T, DA, DT)) 
    : (T = [arbol2,arbol4,arbol3,arbol1], DA = 20) + not_fails
    #"Test 5.8: De un árbol a los otros tres".


% A = 1 T = [2,4] V = 5 DA = 74
%   1 -> 2 (V = 4 DT = 92) -> 4 (V = 0 DT = 112) -> P DT = 146 
%
%

:- pred (movimiento_desde_arbol(T, A, V, DA, DT)) 
    :: (lista_de_arboles(T), arbol(A), number(V), number(DT))
    #"It is true if and only if @var{DT} is the number of time units elapsed from the start of watering 
    until the robot has watered all the trees in the sequence @var{T}. It is defined by: 
    @includedef{movimiento_desde_arbol/5}".

movimiento_desde_arbol([H|T], A, V, DA, DT) :-
    (necesita(H, V2), V >= V2 -> 
            regar_otro_arbol(A, H, V, NV, DA, ND),
            movimiento_desde_arbol(T, H, NV, ND, DT)
        ;    
            V = 0, movimiento_desde_arbol_r([H|T],A,V,DA,DT)
        ).

movimiento_desde_arbol([], A, _V, DA, DT) :-
    movimiento_desde_pozo([A], DA, DT).

:- pred movimiento_desde_arbol_r(L, A, V, DA, DT)
    :: (list(L), arbol(A), number(V), number(DA), number(DT))
    #"Auxiliary predicate which helps predicate @pred{movimiento_desde_arbol/5}, represents when the
    robot needs to return the well. It is defined by:
    @includedef{movimiento_desde_arbol_r/5}".

movimiento_desde_arbol_r([H|T], A, _V, DA, DT) :-
    movimiento_desde_pozo([A], 0, N),
    T1 is DA + N,
    movimiento_desde_pozo([H], T1, NT),
    necesita(H, VH),
    capacidad(C),
    NV is C - VH,
    movimiento_desde_arbol(T, H, NV, NT, DT). 

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1], A = arbol2, V = 10, DA = 20) + not_fails
    #"Test 6.1: De un árbol a otro".

:-test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol2], A = arbol1, V = 10, DA = 20) + not_fails
    #"Test 6.2: De un árbol a otro (en sentido inverso)".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol3], A = arbol1, V = 10, DA = 20) + not_fails
    #"Test 6.3: De un árbol a otro".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol4], A = arbol1, V = 10, DA = 20) + fails
    #"Test 6.4: De un árbol a otro, movimiento no posible".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3], A = arbol2, V = 10, DA = 20) + not_fails
    #"Test 6.5: De un árbol a otros dos".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3,arbol4], A = arbol2, V = 10, DA = 20) + not_fails
    #"Test 6.6: De un árbol a los otros tres".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3,arbol4], A = arbol2, V = 8, DA = 20) + not_fails
    #"Test 6.7: De un árbol a los otros tres".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol4,arbol3,arbol1], A = arbol2, V = 8, DA = 20) + not_fails
    #"Test 6.8: De un árbol a los otros tres".

:- test (movimiento_desde_arbol(T,A,V,DA,DT))
    : (T = [arbol1,arbol3,arbol4], A = arbol2, V = 7, DA = 20) + fails
    #"Test 6.11: Movimiento no posible".

:- pred (trayectoria_valida(A,D,T)) 
    :: (lista_de_arboles(A), number(D), lista_de_arboles(T))
    #"It is true if and only if @var{T} is a valid watering trajectory of duration @var{D} time units, 
    created from the trees belonging to list @var{A} . A watering trajectory is defined as a sequence 
    of tree identifiers indicating the order in which the robot has watered the trees in it. It is defined by:
    @includedef{trayectoria_valida/3}".

:- doc(num_solutions/2, "Counts solutions of a predicate. Used in test cases.").

num_solutions(F, N) :-
    findall(_, F, S),
    length(S, N).

trayectoria_valida(A, D, T) :-
    permute(A, T),
    movimiento_desde_pozo(T, 0, D).

:- test (trayectoria_valida(A, D, T))
    : (A = [arbol1,arbol4]) + fails
    #"Test 7.5: Dos árboles para los que no hay trayectoria".

:- test (trayectoria_valida(A, D, T))
    : (A = [arbol4,arbol1]) + fails
    #"Test 7.6: Dos árboles para los que no hay trayectoria".

:- test (trayectoria_valida(A, D, T))
    : (A = [arbol1,arbol2,arbol3,arbol4])
    => (D=133,T=[arbol1,arbol2,arbol3,arbol4];
    D=131,T=[arbol1,arbol3,arbol2,arbol4];
    D=146,T=[arbol2,arbol1,arbol3,arbol4];
    D=131,T=[arbol2,arbol3,arbol1,arbol4];
    D=127,T=[arbol2,arbol4,arbol1,arbol3];
    D=127,T=[arbol2,arbol4,arbol3,arbol1];
    D=146,T=[arbol3,arbol1,arbol2,arbol4];
    D=133,T=[arbol3,arbol2,arbol1,arbol4];
    D=127,T=[arbol4,arbol2,arbol1,arbol3];
    D=127,T=[arbol4,arbol2,arbol3,arbol1])
    #"Test 7.8: Trayectoria válida entre los 4 árboles, finca del enunciado (probar 4 soluciones)".

:- test (trayectoria_valida(A, D, T))
    : (A = [arbol1,arbol2,arbol3,arbol4] )
    => (num_solutions(trayectoria_valida([arbol1,arbol2,arbol3,arbol4], D, T), 10))
    #"Test 7.9: Trayectoria válida entre los 4 árboles, finca del enunciado, todas las soluciones (deben ser 10)".

/* :- test (trayectoria_valida(A, D, T))
    : (D=133,T=[arbol1,arbol2,arbol3,arbol4];
    D=131,T=[arbol1,arbol3,arbol2,arbol4])
    #"Test 7.8: Trayectoria válida entre los 4 árboles, finca del enunciado (probar 4 soluciones)". */

:- pred (riego(T,D)) 
    : (var(T), var(D)) 
    => (lista_de_arboles(T), number(D))
    #"is true if and only if @var{T} is a valid watering trajectory that takes D units of time and is 
    optimal, in order to maximising the duration of watering. It is defined by:
    @includedef{riego/2}".

riego(T, D) :-
    lista_de_arboles(LT),
    findall(D1, trayectoria_valida(LT, D1, _), S),
    max_list(S, D),
    nth(N, S, D),
    findall(T1, trayectoria_valida(LT, _, T1), S2),
    nth(N, S2, T).

:- test (riego(T, D))
    => (D = 146, T = [arbol2,arbol1,arbol3,arbol4];
    D = 146, T = [arbol3,arbol1,arbol2,arbol4])
    #"Test 8.1: Trayectorias optimas (en la finca del enunciado)".

:- test (riego(T,D))
    => (num_solutions(riego(T,D), 2))
    #"Test 8.2: Trayectorias optimas (en la finca del enunciado, todas las soluciones, deben ser 2)".


/* :- test (riego(T, D))
    => num_solutions(riego(T, D), 2), member(T, [[arbol2,arbol1,arbol3,arbol4],[arbol3,arbol1,arbol2,arbol4]])
    #"Test 8.2: Generar trayectorias y duraciones, finca del enunciado (al menos 2 soluciones)". */

%------------------------------------------------------------------------------------%