% idiotsort(?X,?Y).
% *idiotsort/2 triunfa si Y es la lista ordenada de X
idiotsort([],[]).
idiotsort(Lista,Ordenada) :- 
    list(Ordenada),
    permutation(Ordenada,Lista). 
idiotsort(Lista,Ordenada) :- 
    permutation(Lista,Ordenada), 
    ordenada(Ordenada),!.

% ordenada(?X).
% *ordenada/1 triunfa si la lista X esta ordenada
% *de menor a mayor 
ordenada([_]). 
ordenada([X,Y|Ys]) :- 
    X=<Y, 
    ordenada([Y|Ys]).
