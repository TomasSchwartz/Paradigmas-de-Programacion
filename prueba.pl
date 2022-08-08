atiende(dodain,lunes,9,15).
atiende(dodain,miercoles,9,15).
atiende(dodain,viernes,9,15).

atiende(lucas,martes,10,20).

atiende(juanC,sabado,18,22).
atiende(juanC,domingo,18,22).

atiende(juanFdS,jueves,10,20).
atiende(juanFdS,viernes,12,20).

atiende(leoC,lunes,14,18).
atiende(leoC,miercoles,14,18).

atiende(martu,miercoles,23,24).

ventas(dodain,fecha(10,8),[golosinas(1200),cigarrillo(jockey),golosina(50)]).
ventas(dodain,fecha(12,8),[bebida(true,8),bebida(false,1),golosinas(10)]).
ventas(bebidas(Alcoholica,Cantidad)).


atiende(vale,Dias,Principio,Fin):-
    atiende(dodain,Dias,Principio,Fin).

atiende(vale,Dias,Principio,Fin):-
    atiende(juanC,Dias,Principio,Fin).

atiendeEnHorario(Persona,Dia,Horario):-
    atiende(Persona,Dia,Principio,Fin),
    between(Principio,Fin,Horario).

atiendeSolo(Persona,Dia,Horario):-
    atiendeEnHorario(Persona,Dia,Horario),
    not(atiendeEnHorario(Persona2,Dia,Horario),
    Persona\=Persona2).

posibilidadPorDia(Dia,Personas):-
    findall(Persona,atiendeEnHorario(Persona,Dia, _),Personas).




