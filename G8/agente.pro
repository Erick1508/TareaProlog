:-op(100,yfx,:).

% esGrande(?X).
% *esGrande/1 triunfa para aquellas ciudades que son consideradas grandes.
esGrande(new_york).
esGrande(chicago).
esGrande(los_angeles).
esGrande(san_francisco).

% esMediano(?X).
% *esMediano/1 triunfa para aquellas ciudades que son consideradas medianas.
esMediano(san_francisco).
esMediano(dallas).
esMediano(miami).

% esPequenio(?X).
% *esPequenio/1 triunfa para aquellas ciudades que NO son consideradas ni 
% grandes ni medianas.
esPequenio(X) :- 
   \+ esGrande(X), 
   \+ esMediano(X).

% horario(?X,?Y,?Z).
% *Este predicado es considerado como una mini base de datos para expresar
% los vuelos con X una ciudad de origen, Y ciudad de destino, y Z es
% una lista de vuelos de la siguiente forma:
% [HoraSale/HoraLLega/NroVuelo/Dias], donde HoraSale es la hora en la que 
% sale el vuelo, HoraLLega es la hora en la que llega al destino, NroVuelo
% es el número asociado al vuelo a realizar y Dias son los días en los
% que se realiza dicho vuelo.
horario( new_york, chicago,
           [  9:40 / 10:50 / nw4733 / todos,
             13:40 / 14:50 / nw4773 / habiles,
             19:40 / 20:50 / nw4833 / [lun,mar,mie,jue,vie,dom] ] ). 
             
horario( chicago, new_york,
           [  9:10 / 10:00 / nw458 / todos,
             12:20 / 13:10 / aa511 / todos ] ). 

horario( chicago, dallas,
           [  9:40 / 10:50 / aa4732 / todos,
             11:40 / 12:50 / aa4752 / habiles,
             18:40 / 19:50 / aa4822 / [lun,mar,mie,jue,vie] ] ). 

horario( dallas, los_angeles,
           [ 13:20 / 16:20 / nw212 / [lun,mar,mie,vie,dom],
             16:30 / 19:30 / aa473 / [lun,mie,jue,sab] ] ). 

horario( new_york, washington,
           [  9:10 / 11:45 / united614 / todos,
             14:45 / 17:20 / united805 / todos ] ). 

horario( chicago, miami,
           [  8:30 / 11:20 / nw510 / todos,
             11:00 / 13:50 / aa459 / todos ] ). 

horario( los_angeles, san_francisco,
           [ 11:30 / 12:40 / sw322 / [mar,jue] ] ). 
           
horario( san_francisco, los_angeles,
           [  9:25 / 10:15 / aa621 / todos,
             12:45 / 13:35 / sw623 / todos ] ). 

horario( san_francisco, seattle,
           [ 11:10 / 12:20 / sw211 / [lun,mar,mie,vie,dom],
             20:30 / 21:30 / nw472 / [lun,mie,jue,sab] ] ). 

horario( seattle, san_francisco,
           [ 7:55 / 8:45 / aa620 / todos,
             11:25 / 12:15 / aa666 / habiles ] ).

horario( dallas, san_francisco,
           [ 13:30 / 14:40 / nw323 / [mar,jue] ] ). 

horario( boston, new_york,
           [ 9:00 / 9:40 / aa613 / [lun,mar,mie,jue,vie,sab],
            16:10 / 16:55 / united806 / [lun,mar,mie,jue,vie,dom] ] ). 

horario( boston, chicago,
           [ 9:00 / 9:40 / aa613 / [lun,mar,mie,jue,vie,sab],
            16:10 / 16:55 / united806 / [lun,mar,mie,jue,vie,dom] ] ). 
            
% m(?X,?Y).
% *m/2 triunfa si X esta en la Lista Y, y si además Y es una lista
% *lo busca en sus otros niveles. (multinivel).
m(X,[K|_]):-
    list(K),m(X,K).
m(X,[X|_]).
m(X,habiles) :- 
    m(X,[lun,mar,mie,jue,vie]).
m(X,[habiles]) :- 
    m(X,[lun,mar,mie,jue,vie]).
m(X,todos) :- 
    m(X,[lun,mar,mie,jue,vie,sab,dom]).
m(X,[todos]) :- 
    m(X,[lun,mar,mie,jue,vie,sab,dom]).
m(X,[_Y|R]) :- 
    m(X,R).

% numdia(?X,?Y).
% *numdia/2 triunfa si X es un dia para luego unificar Y con un 
% *número de la semana y determinar cierta suma de dias.
numdia(lun,Lun):- Lun = 0,!.
numdia(mar,Mar):- Mar = 1,!.
numdia(mie,Mie):- Mie = 2,!.
numdia(jue,Jue):- Jue = 3,!.
numdia(vie,Vie):- Vie = 4,!.
numdia(sab,Sab):- Sab = 5,!.
numdia(dom,Dom):- Dom = 6,!.

% rutaDirecta(?X,?Y,Z?,?W).
% *rutaDirecta/4 triunfa si W es la Ruta que se puede tomar desde la 
% ciudad X hasta la ciudad Y sin nodos intermedios, y además Z es el
% dáa que se puede tomar dicha ruta.
rutaDirecta(Origen,Origen,_,[]).
rutaDirecta(Origen,Destino,Dia,[Origen,Destino,Ruta]):-
    horario(Origen,Destino,Horario),
    valoRuta(Dia,Horario,Ruta).

% ruta(?X,?Y,Z?,?W).
% *ruta/4 triunfa si W es la Ruta que se puede tomar desde la 
% ciudad X hasta la ciudad Y pudiendo atravesar ciudades intermedias,
% y además Z es el día que se puede tomar dicha ruta.
ruta([],_,_,[]).
ruta(_,[],_,[]).
ruta(Origen,Origen,_,[]).
ruta(Origen,Destino,Dia,[Origen,Destino,Ruta]):-
    horario(Origen,Destino,Horario),
    valoRuta(Dia,Horario,Ruta).
ruta(Origen,Destino,Dia,Ruta):-
    camino(Origen,Destino,Camino),
    generarHorario(Camino,Horarios),
    verificarDias(Dia,Horarios,RutaParcial),
    chequeoHoras(Dia,RutaParcial,Ruta,[]).

% -------------------------------------------------------------------------------- %
% ----------------------------- Predicados para ruta ----------------------------- %
% -------------------------------------------------------------------------------- %

% valoRuta(?X,?Y,Z?).
% *valoRuta/3 triunfa si eso ocurre Z es el horario Y esperado, sabiendo que
% X es un día, que pertenece a alguno de los vuelos de Y.
valoRuta(_,[],[]).
valoRuta(D,Horario,R):- 
    listaDias(Horario,Dias),
    habilesYtodos(Dias,DiasCompleta),
    listaAtomos(DiasCompleta,SoloAtomos),
    eliminaR(SoloAtomos,SinRepetidos),
    m(D,SinRepetidos),
    R = Horario.

% habilesYtodos(?X,?Y).
% *habilesYtodos/2 triunfa si Y es la lista X solo que si se encuentran los atomos hábiles y
% todos, son sustituidos por [lun,mar,mier,jue,vie] y [lun,mar,mier,jue,vie,sab,dom]
% *respectivamente.
habilesYtodos([],[]). 
habilesYtodos([X|Y],[[lun,mar,mier,jue,vie]|R]):-
    atom(X), 
    X = habiles,
    habilesYtodos(Y,R).
habilesYtodos([X|Y],[[lun,mar,mier,jue,vie,sab,dom]|R]):-
    atom(X), 
    X = todos,
    habilesYtodos(Y,R).
habilesYtodos([X|Y],[X|R]):-
    habilesYtodos(Y,R).

% listaAtomos(?X,?Y).
% *listaAtomos/2 triunfa si Y es la lista de los átomos que se encuentren en X, esto es, si
% *X es multinivel, Y es de un sólo nivel.
listaAtomos([],[]):-!. 
listaAtomos([X|Y],[X|Ys]):- 
    atom(X),listaAtomos(Y,Ys),!. 
listaAtomos([X|Y],Ys):- 
    \+(atom(X)),
    listaAtomos(X,AtomosX),
    append(AtomosX,Y,Z),
    listaAtomos(Z,Ys). 
 
% listaDias(?X,?Y).
% *listaDias/2 triunfa si Y son los días en los que ocurren los vuelos X
listaDias([],[]).
listaDias([_X/_A/_V/B|Y],[B|R]):-
    listaDias(Y,R).
            
% camino(?X,?Y,?Z).
% *camino/3 triunfa si Z es la lista de nodos o ciudades por las que deben pasar
% *X para llegar a Y.
camino(Start,Stop,Camino) :-
    camino1(Start,Stop,[Start],W), 
    reverse(W,Camino).
 
% camino1(?X,?Y,?Z).
% *camino1/3 triunfa si Z es la lista de nodos o ciudades por las que deben pasar
% *X para llegar a Y en orden inverso.
camino1(Start,Stop,ActCamino,Camino) :-
    Start\=Stop,
    horario(Start,Proximo,_Horario),
    non_miembro(Proximo,ActCamino),
    camino1(Proximo,Stop,[Proximo|ActCamino],Camino).
 
% non_miembro(?X,?Y).
% *non_miembro/2 triunfa si X NO es miembro o no pertenece a la lista Y.
non_miembro(_,[]).
non_miembro(X,[Y|T]) :-
    X\=Y,
    non_miembro(X,T).

% generarHorario(?X,?Y).
% *generarHorario/2 triunfa si Y es la lista de [Origen,Destino,Horario], que se 
% genera a través de viajar entre las ciudades de la lista X, note que X es una
% *lista de al menos dos ciudades.
generarHorario([],[]).            
generarHorario([A,B],[[A,B,Horario]]):-
    horario(A,B,Horario).
generarHorario([A,B|C],[[A,B,Horario]|Siguiente]):-
    horario(A,B,Horario),
    generarHorario([B|C],Siguiente),!.

% verificarDias(?X,?Y,?Z).
% *verificarDias/3 triunfa si Z es la lista de [Origen,Destino,Horario], que se 
% *genera comparando si X (un dia) pertenece al Horario de Y. 
verificarDias(_,[],[]).
verificarDias(Dia,[O,D,X],[O,D,X]) :- 
    listaDias(X,Dias),
    m(Dia,Dias).
verificarDias(Dia,[[O,D,X]|Y],[[O,D,X]|Z]) :- 
    listaDias(X,Dias),
    m(Dia,Dias), 
    verificarDias(Dia,Y,Z).

% chequeoHoras(?X,?Y,?Z,?W).
% *chequeoHoras/4 triunfa si en el día X se puede viajar desde las ciudades Origen
% y Destino de Y y Destino,SegundoDestino, y además si cumplen los parámetros 
% de tiempo pertinentes. Z es la Ruta por la que se debería viajar, y W es el 
% vuelo en cuestión previo en la recursión para no hacer chequeos dobles.
chequeoHoras(_,[],[],_):-!.
chequeoHoras(Dia,[[Origen,Destino,Vuelos],[Destino,SegundoDestino,Vuelos1]],
            [Origen,Destino,VueloTentativo,Destino,SegundoDestino,VueloTentativo1],
            ContinuaVuelo):-
    vuelosRestantes(Vuelos,ContinuaVuelo,Restantes),
    verificarVuelo(Dia,Restantes,VueloTentativo),
    verificarVuelo(Dia,Vuelos1,VueloTentativo1),
    chequearTiempo(Destino,VueloTentativo,VueloTentativo1).  
chequeoHoras(Dia,[[Origen,Destino,Vuelos],[Destino,SegundoDestino,Vuelos1]|Y],
            [Origen,Destino,VueloTentativo|R],ContinuaVuelo):-
    vuelosRestantes(Vuelos,ContinuaVuelo,Restantes),
    verificarVuelo(Dia,Restantes,VueloTentativo),
    verificarVuelo(Dia,Vuelos1,VueloTentativo1),   
    chequearTiempo(Destino,VueloTentativo,VueloTentativo1),
    chequeoHoras(Dia,[[Destino,SegundoDestino,Vuelos1]|Y],R,VueloTentativo1).

% vuelosRestantes(?X,?Y,?Z).
% *vuelosRestantes/3 triunfa si Z es la lista de vuelos restantes que resulta
% *eliminar los vuelos anteriores a Y de X.
vuelosRestantes([],VueloContinua,VueloContinua).
vuelosRestantes(T,[],T).
vuelosRestantes(VuelosTotal,VueloContinua,Resto):-
    firstElem(VueloContinua,V),
    append(_W,[V|O],VuelosTotal),
    append(VueloContinua,O,Resto).   
    
% firstElem(?X,?Y).
% *firstElem/2 triunfa si Y es el primer elemento de X. 
firstElem([_X|_Y],_X).   

% verificarVuelo(?X,?Y,?Z).
% *verificarVuelo/3 triunfa si Z es un vuelo que está en Y que cumple
% *que puede ser efectuado el día X.
verificarVuelo(_,[],[]):-fail.
verificarVuelo(Dia,[S/LL/N/D | _Resto],[S/LL/N/D]):-
    m(Dia,D).
verificarVuelo(Dia,[_S/_LL/_N/_D | Resto],L):-
    verificarVuelo(Dia,Resto,L).

    
% chequearTiempo(?X,?Y,?Z).
% *chequearTiempo/3 triunfa si los horarios de llegada en Y y salida en Z de 
% *la ciudad ciudad X no están fuera de las horas especificadas en el enunciado.
% Cumplen con características de tamaño con respecto a la ciudad X, bien
% sea grande, pequeña o mediana.
chequearTiempo(_,[],[]).
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esGrande(Ciudad),
    M<30,
    M2 is M+30,
    H2 is H+1,
    M1 >= M2,
    H1 >= H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esGrande(Ciudad),
    M<30,
    M2 is M+30,
    H2 is H+1,
    M1 < M2,
    H1 > H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esGrande(Ciudad),
    M>=30,
    M2 is M-30,
    H2 is H+2,
    M1 >= M2,
    H1 >= H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esGrande(Ciudad),
    M>=30,
    M2 is M-30,
    H2 is H+2,
    M1 < M2,
    H1 > H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esMediano(Ciudad),
    H2 is H+1,
    M1 >= M,
    H1 >= H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esMediano(Ciudad),
    H2 is H+1,
    M1 < M,
    H1 > H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esPequenio(Ciudad),
    M < 20,
    M2 is M+40,
    M1 >= M2,
    H1 >= H.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esPequenio(Ciudad),
    M >= 20,
    M2 is M+40-60,
    H2 is H+1,
    M1 >= M2,
    H1 >= H2.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esPequenio(Ciudad),
    M < 20,
    M2 is M+40,
    M1 < M2,
    H1 > H.
chequearTiempo(Ciudad,[_S/H:M/_N/_],[H1:M1/_LL1/_N1/_]):-
    esPequenio(Ciudad),
    M >= 20,
    M2 is M+40-60,
    H2 is H+1,
    M1 < M2,
    H1 > H2.

% eliminaR(?X,?Y).
% *eliminaR/2 triunfa si Y es la lista de elementos de X sin repetidos.
eliminaR([],[]):-!. 
eliminaR([X|M],S):- 
    \+(list(X)),
    eliminaX(M,X,T),
    eliminaR(T,Y),
    append([X],Y,S). 
eliminaR([X|M],S):-
    list(X),
    elim12(X,M,T),
    eliminaR(X,Y), 
    eliminaR(T,J),
    append([Y],J,S). 
    
 
% eliminaX(?X,?Y,?Z).
% *eliminaX/3 triunfa si Z es la lista de elementos de X sin el elemento Y,
% *en el primer nivel.
eliminaX([],_X,[]):-!. 
eliminaX([X|M],X,Z):- 
    eliminaX(M,X,Z),!. 
eliminaX([R|M],X,[R|Z]):- 
    eliminaX(M,X,Z),!. 
    

% elim12(?X,?Y,?Z).
% *elim12/2 triunfa si Z es la lista de elementos que resulta de eliminar
% *los elementos de Y que estan en X.
elim12([],L,L):-!. 
elim12([X|M],L,S):-
    eliminaMx(L,X,T),
    elim12(M,T,S). 

% Elimina el elemento X de la lista en el 1º nivel
% eliminaMx(?X,?Y,?Z).
% *eliminaMx/3 triunfa si Z es la lista de elementos de X sin el elemento Y,
% *en el TODOS los niveles.
eliminaMx([],_X,[]):-!. 
eliminaMx([X],X,[]):-!. 
eliminaMx([X|M],X,S):-
    eliminaMx(M,X,S),!. 
eliminaMx([R|M],X,S):-
    list(R),
    eliminaMx(R,X,T),
    eliminaMx(M,X,P),
    append([T],P,S),!. 
eliminaMx([R|M],X,S):-
    eliminaMx(M,X,T),
    append([R],T,S). 

% -------------------------------------------------------------------------------- %
% --------------------------- Fin Predicados para ruta --------------------------- %
% -------------------------------------------------------------------------------- %

% -------------------------------------------------------------------------------- %
% ----------------------------- Predicados para gira ----------------------------- %
% -------------------------------------------------------------------------------- %

% gira(?X,?Y,?Z).
% *gira/3 triunfa si Z es la ruta de ir por las
% *ciudades X con Y días de parada
gira(_,_,Ruta):- atom(Ruta),!,fail.
gira(Ciudades,Dias,Ruta) :- giraaux(Ciudades,Dias,_,Ruta,_),!.

% giraaux(?V,?W,?X,?Y,?Z).
% *gira/5 triunfa si Y es la ruta de ir por las
%  ciudades V con X dias de parada, W funciona como
% *acumulador de estas con con Z días de parada.
giraaux([],_,_,_).
giraaux([_],_,Ruta1,Ruta,_) :- append(Ruta1,[],Ruta).
giraaux([C1,C2|C3],Dias,Rutaacum,Ruta,DiadeViaje):-
    ruta(C1,C2,DiadeViaje,Ruta1),
    sumarDias(Dias,DiadeViaje,DiadeVuelo),
    append(Rutaacum,Ruta1,Ruta2),!,
    giraaux([C2|C3],Dias,Ruta2,Ruta,DiadeVuelo).

% sumarDias(?X,?Y,?Z).
% *sumarDias/3 triunfa si Z es la suma de 
% *Y mas los días de retorno X
sumarDias(Estadia,Seguirviaje,DiaRetorno):- 
    numdia(Seguirviaje,Dia),
    Diasobrante is Dia+Estadia,
    NumDiaRetorno is mod(Diasobrante,7),
    numdia(DiaRetorno,NumDiaRetorno).
    

    
% -------------------------------------------------------------------------------- %
% ----------------------------- Fin Predicados para gira ----------------------------- %
% -------------------------------------------------------------------------------- %


% -------------------------------------------------------------------------------- %
% ------------------------------------- MENÚ ------------------------------------- %
% -------------------------------------------------------------------------------- %
% pausa &lt- detiene la ejecución del programa hasta que se pulse una tecla
pausa :- 
    nl,
    write('Pulsa <ENTER> para continuar '),
    skip(10).

% borraPantalla &lt- borra la pantalla
borraPantalla :- 
    borraLinea(25).
borraLinea(1) :- 
    !,nl.
borraLinea(N) :- 
    nl,
    N1 is N-1,
    borraLinea(N1).

%-------------------------Muestra mensaje de error---------------------------------
error:-
    write('No escribió un número'), nl,
    write('O el número escrito no está en el rango del Menu'),nl.

%-------------------------Mensaje de Salida---------------------------------
salida:-
    borraPantalla,
    write('|----------------------------------------------------|'),nl,
    write('|-----------Saliendo del Agente de Viajes------------|'),nl,
    write('|----------------------------------------------------|'),nl,
    pausa,
    halt.

% Predicado para imprimir una Ruta y su Día
imprimir(D,R) :- 
    write('Día = '),
    write(D),nl,
    write('Ruta = '),
    write(R),nl,
    fail.

% Predicado para imprimir una Ruta    
imprimir(R) :- 
    write('Ruta = '),
    write(R),nl,
    fail.

%-------------------------Manejo de opciones Menu Principal---------------------------------
opciones(X):- 
    X =:= 1, write('Seleccionó la Opción 1'),nl,
    write('Indique Ciudad A'),nl,
    read(CiudadA),
    write('Indique Ciudad B'),nl,
    read(CiudadB),
    write('Indique que Día desea ir'),nl,
    read(Dia),
    write('El día \''),write(Dia),write('\' Se puede viajar de las siguientes formas: '),nl,
    ruta(CiudadA,CiudadB,Dia,Rutas),
    imprimir(Rutas),
    menu.

opciones(X):- 
    X =:= 2,write('Seleccionó la Opción 2'),nl,
    write('Indique Ciudad A'),nl,
    read(CiudadA),
    write('Indique Ciudad B'),nl,
    read(CiudadB),
    write('Se puede viajar de las siguientes formas: '),nl,
    ruta(CiudadA,CiudadB,Dia,Rutas),
    imprimir(Dia,Rutas),
    menu.
	      
opciones(X):- 
    X =:= 3,write('Seleccionó la Opción 3'),nl,
    write('Indique Ciudad A'),nl,
    read(CiudadA),
    write('Indique Ciudad B'),nl,
    read(CiudadB),
    write('Se puede viajar de las siguientes formas: '),nl,
    rutaDirecta(CiudadA,CiudadB,Dia,Rutas),
    imprimir(Dia,Rutas),
    menu.
	      
opciones(X):- 
    X =:= 4,write('Seleccionó la Opción 4'),nl,
    write('Indique Ciudad A'),nl,
    read(CiudadA),
    write('Indique Ciudad B'),nl,
    read(CiudadB),
    write('Indique los días de paradas'),nl,
    read(Dia),
    write('Se puede viajar de las siguientes formas: '),nl,
    gira([CiudadA,CiudadB],Dia,Rutas),
    imprimir(Rutas),
    menu.

opciones(X):- 
    X =:= 5,write('Seleccionó la Opción 5'),nl,
    write('Indique Ciudad A'),nl,
    read(CiudadA),
    write('Indique Ciudad B'),nl,
    read(CiudadB),
    write('Indique los días de paradas'),nl,
    read(Dia),
    write('Se puede viajar de las siguientes formas: '),nl,
    gira([CiudadA,CiudadB,CiudadA],Dia,Rutas),
    imprimir(Rutas),
    menu.
	      
opciones(X):- 
    X =:= 6,
    salida.
	      
opciones(X):- 
    X>6, X<1, 
    error.

opciones(X):- 
    X=<6, 
    X>=1, nl.

opciones(_X):- 
    error.

%-------------------------Menú Principal---------------------------------
menu:-
    write('|------------Menú principal------------------|'),nl,
    write('|-----------Agente de Viajes-----------------|'),nl,
    write('|------------<<IMPORTANTE>>------------------|'),nl,
    write('|--Cada opción/acción que se desee realizar--|'),nl,
    write('|------debe terminar con el \'.\', de lo-------|'),nl,
    write('|-------contrario no se ejecutará nada-------|'),nl,nl,
    write('1) ¿Cómo puedo ir desde Ciudad A hasta Ciudad B?'),nl,
    write('2) ¿En cúales días puedo viajar desde A hasta B?'),nl,
    write('3) ¿En cúales días puedo viajar directamente entre A y B?'),nl,
    write('4) ¿Cómo puedo hacer una gira entre A y B, con paradas de N días?'),nl,
    write('5) ¿Cómo puedo hacer un viaje de ida y vuelta entre A y B, posiblemente con paradas de N días?'),nl,
    write('6) Salir'),nl,
    write('Digite su opción:'), 
    read(X),nl,
    opciones(X),
    menu.


