mago(harry,mestiza,[coraje,amistoso,orgullo,inteligencia],slytherin).
mago(draco,pura,[inteligencia,orgullo],hufflepuff).
mago(hermione,impura,[inteligencia,orgullo,responsabilidad],_).

casa(gryffindor,coraje).
casa(slytherin,orgullo).
casa(slytherin,inteligencia).
casa(ravenclaw,inteligencia).
casa(ravenclaw,responsabilidad).
casa(hufflepuff,amistoso).

permiteEntrar(Mago,Casa):-
    mago(Mago,_,_,_).

permiteEntrar(slytherin,Mago):-
    not(mago(Mago,mestiza,_,_)).

tieneCaracterApropiado(Mago,Casa):-
    mago(Mago,_,Propiedades,_),
    casa(Casa,_),
    forall(casa(Casa,Propiedad),member(Propiedad,Propiedades)).

puedeQuedarEnCasa(Mago,Casa):-
    tieneCaracterApropiado(Mago,Casa),
    permiteEntrar(Casa,Mago),
    not(magoOdiaCasa(Mago,Casa)).

puedeQuedarEnCasa(hermione,gryffindor).

magoOdiaCasa(Mago,Casa):-
    mago(Mago,_,_,Casa).

cadenaDeAmistades(Magos):-
    forall(Magos,esAmistoYCasa(Magos,Casa)).

esAmistoYCasa(Mago,Casa):-
    mago(Mago,_,Propiedades,_),
    member(amistoso,Propiedades),
    puedeQuedarEnCasa(Mago,Casa).

accion(harry,fueraDeCama,-50).
accion(harry,tercerPiso,-75).
accion(harry,bosque,-50).
accion(hermione,tercerPiso,-75).
accion(draco,_).
accion(hermione,bibliotecaRestringida,-10).
accion(ron,buenaAccion,50).
accion(hermione,buenaAccion,50).
accion(harry,buenaAccion,60).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

esBuenAlumno(Mago):-
    accion(Mago,_,_),
    not((
        accion(Mago,Accion,_),
        accionMala(Accion))).

accionMala(fueraDeCama).
accionMala(tercerPiso).
accionMala(bosque).
accionMala(bibliotecaRestringida).

accionRecurrente(Accion):-
    accion(Mago,Accion,_),
    accion(OtroMago,Accion,_),
    Mago\=OtroMago,
    Accion\=buenaAccion.

puntaje(Casa,TodosLosPuntos):-
    forall(esDe(Casa,Miembro),findall(Puntos,accion(Miembro,Accion,Puntos),PuntosTotales)),
    sumlist(PuntosTotales, TodosLosPuntos).
    
    