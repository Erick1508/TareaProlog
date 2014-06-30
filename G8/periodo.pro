% periodo(?X,?Y,?Z).
% *periodo/3 triunfa si Z es la lista de los
%  números que representan el periodo de la
% *división entre X e Y
periodo(_,_,Periodo) :- atom(Periodo),!,fail.
periodo(Dividendo,Divisor,Periodo) :- 
    Dividendo >= 0,
    Divisor > 0,
    periodoaux(Dividendo,Divisor,[],Periodo).

% periodoaux(?W,?X,?Y,?Z).
% *periodoaux/4 triunfa si Y contiene un periodo
% asociado con W y X, y es igual al periodo Z
% * o lo unifica con Z
periodoaux(_,_,Peri,Periodo) :- 
    chequearper(Peri,Periodo),!.
periodoaux(_,_,Peri,Periodo) :- 
    \+ (length(Periodo,0)),
    length(Peri,Tam1),
    length(Periodo,Tam2),
    (Tam2*2) < Tam1,!, 
    fail.
periodoaux(Dividendo,Divisor,Peri,Periodo) :- 
    Res is mod(Dividendo,Divisor),
    Resto is Res*10,
    Per is Resto//Divisor,
    append(Peri,[Per],Perio),
    Res1 is mod(Resto,Divisor),
    Resto2 is Res1*10,
    Per2 is Resto2//Divisor,
    append(Perio,[Per2],Perioaux),
    \+ (Per == Per2),  
    periodoaux(Resto2,Divisor,Perioaux,Periodo),!.
periodoaux(Dividendo,Divisor,Peri,Periodo) :- 
    Res is mod(Dividendo,Divisor),
    Resto is Res*10,
    Per is Resto//Divisor,
    append(Peri,[Per],Perio),
    Res1 is mod(Resto,Divisor),
    Resto2 is Res1*10,
    Per2 is Resto2//Divisor,
    append(Perio,[Per2],Perioaux),
    Res2 is mod(Resto2,Divisor),
    Resto3 is Res2*10,
    Per3 is Resto3//Divisor,
    append(Perioaux,[Per3],Perioaux1),
    Res3 is mod(Resto3,Divisor),
    Resto4 is Res3*10,
    Per4 is Resto4//Divisor,
    append(Perioaux1,[Per4],Perioaux2),
    Res4 is mod(Resto4,Divisor),
    Resto5 is Res4*10,
    Per5 is Resto5//Divisor,
    append(Perioaux2,[Per5],Perioaux3),
    periodoaux(Resto5,Divisor,Perioaux3,Periodo),!.
  

% chequearper(?X,?Y).
% *chequearper/2 triunfa si X contiene un periodo
% *igual a Y,o lo unifica
chequearper([A|B],Periodo) :- 
    append(Pri,Res,[A|B]),
    append(Pri,[],Res),
    \+ (iguales(Res,[A|B])),
    append(Res,[],Periodo),!. 
chequearper([_|B],Periodo) :- 
    chequearper(B,Periodo).

% iguales(?X,?Y).
% *iguales/2 triunfa si la lista X es igual a la lista Y
iguales([],[]) :- !. 
iguales([A|B],[A|B]) :- 
    iguales(B,B). 

