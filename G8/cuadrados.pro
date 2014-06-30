% diabolico(?X).
% *diabolico/1 triunfa si X es la lista de los
%  números que representan el un cuadrado mágico
% *pandiagonal.
diabolico(Evil):- 
    list(Evil),!,
    length(Evil,N), 
    N =:=16,
    asignarValores(Evil,[N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16]),!,
    F1 = 34,
    F1 is N1 + N2 + N3 + N4,
    F1 is N5 + N6 + N7 + N8,
    F1 is N9 + N10 + N11 + N12,
    F1 is N13 + N14 + N15 + N16,
    F1 is  N1 + N5 + N9 + N13,
    F1 is  N2 + N6 + N10 + N14,
    F1 is  N3 + N7 + N11 + N15,
    F1 is  N4 + N8 + N12 + N16,    
    % Diagonales mayores
    F1 is N1 + N6 + N11 + N16,
    F1 is N4 + N7 + N10 + N13,    
    % Diagonales menores de izquierda a derecha
    F1 is N5 + N10 + N15 + N4,
    F1 is N9 + N14 + N3 + N8,       
    F1 is N13 + N2 + N7 + N12,                                                           
    % Diagonales menores de derecha a izquierda   
    F1 is N8 + N11 + N14 + N1,
    F1 is N12 + N15 + N2 + N5,       
    F1 is N16 + N3 + N6 + N9.                                                        

diabolico(Evil):-
    Matriz = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],
    asignarValores(Matriz,[N1,N2,N3,N4]), % Valores para la Primera Fila
    F1 is N1 + N2 + N3 + N4,
    F1 =:= 34,
    append([N1],[N2],L),
    append([N3],[N4],L1),
    append(L,L1,Fila1),
    
    reducirSoluciones(Matriz,Fila1,MatrizCambia1),
    asignarValores(MatrizCambia1,[N5,N6,N7,N8]), % Valores para la Segunda Fila
    F2 is N5 + N6 + N7 + N8,
    F2 =:= 34,
    append([N5],[N6],L2),
    append([N7],[N8],L3),
    append(L2,L3,Fila2),
  
    reducirSoluciones(MatrizCambia1,Fila2,MatrizCambia2),  
    asignarValores(MatrizCambia2,[N9,N10,N11,N12]), % Valores para la Tercera Fila
    F3 is N9 + N10 + N11 + N12,
    F3 =:= 34,
    append([N9],[N10],L4),
    append([N11],[N12],L5),
    append(L4,L5,Fila3),

    reducirSoluciones(MatrizCambia2,Fila3,MatrizCambia3),  
    asignarValores(MatrizCambia3,[N13,N14,N15,N16]), % Valores para la Cuarta Fila
    F4 is N13 + N14 + N15 + N16,
    F4 =:= 34,
    append([N13],[N14],L6),
    append([N15],[N16],L7),
    append(L6,L7,Fila4),
    
    %Verificaciones para las columnas, y para cada una de las diagonales mayores
    % derecha e izquierda, y menores izquierdas y derechas.
    Col1 is  N1 + N5 + N9 + N13,
    Col1 =:= 34, 
    Col2  is  N2 + N6 + N10 + N14,
    Col2 =:= 34, 
    Col3  is  N3 + N7 + N11 + N15,
    Col3 =:= 34,
    Col4  is  N4 + N8 + N12 + N16,
    Col4 =:= 34,
    
    % Diagonales mayores
    DiagMay1 is N1 + N6 + N11 + N16,
    DiagMay1 =:= 34, 
    DiagMay2 is N4 + N7 + N10 + N13,
    DiagMay2 =:= 34, 
    
    % Diagonales menores de izquierda a derecha
    DiagMenIzq1 is N5 + N10 + N15 + N4,
    DiagMenIzq1 =:= 34, 
    DiagMenIzq2 is N9 + N14 + N3 + N8,       
    DiagMenIzq2 =:= 34, 
    DiagMenIzq3 is N13 + N2 + N7 + N12,                                                    
    DiagMenIzq3 =:= 34,
       
    % Diagonales menores de derecha a izquierda   
    DiagMenDer1 is N8 + N11 + N14 + N1,
    DiagMenDer1 =:= 34, 
    DiagMenDer2 is N12 + N15 + N2 + N5,       
    DiagMenDer2 =:= 34, 
    DiagMenDer3 is N16 + N3 + N6 + N9,                                                    
    DiagMenDer3 =:= 34,

    % Despues de pasar TODOS los chequeos, se concatenan las filas, y se unifican con Evil
    append(Fila1,Fila2,Aux),
    append(Fila3,Fila4,Aux1),
    append(Aux,Aux1,Evil).
    
    
% asignarValores(?X,?Y).
% *asignarValores/2 triunfa si Y es la lista de los
%  números que son unificados con elementos de la Lista X.
% X es una Lista de elementos entre [1..16] y se va reduciendo
% para evitar tener valores repetidos.
asignarValores(_,[]).
asignarValores(X,[W|Z]) :- 
    asignarElem(X,W), 
    eliminar(W,X,Y),
    asignarValores(Y,Z).

% reducirSoluciones(?X,?Y,?Z).
% *reducirSoluciones/3 triunfa si Z es la lista de los
%  números que resultan de eliminar los elementos de Y
%  de la lista X. 
reducirSoluciones(X,[],X).
reducirSoluciones(X,[W|Z],K) :- 
    eliminar(W,X,SinW),
    reducirSoluciones(SinW,Z,K).

% *asignarElem(?X,?Y).
% *asignarElem/2 triunfa si Y es un elemento unificado de la
% lista X.
asignarElem([X|_],X).
asignarElem([_X|Z],Y) :- 
    asignarElem(Z,Y).

% *eliminar(?X,?Y,?Z).
% *eliminar/3 triunfa si Z es la lista de elemnos Y
% habiendo eliminado el elemento X de Y.
eliminar(_,[],[]):-fail. 
eliminar(X,[X|R],R). 
eliminar(X,[C|R],[C|R1]):- 
    eliminar(X,R,R1).

% *stopwatch(?Predicate).
% *stopwatch/1 calcula el tiempo de ejecución del Predicate
stopwatch(Predicate) :-
    real_time(Start),
    call(Predicate),
    real_time(Finish),
    Elapsed is (Finish - Start) / 1000,
    format('~4f seg~N',[Elapsed]), !.        
        
