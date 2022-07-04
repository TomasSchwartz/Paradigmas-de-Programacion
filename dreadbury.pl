
mata(Asesino,UnaPersona):-
    odia(Asesino,UnaPersona),
    viveEnMansion(Asesino),
    not(masRico(Asesino,UnaPersona)).


viveEnDreadbury(tiaAgatha).
viveEnDreadbury(mayordomo).
viveEnDreadbury(charles).

persona(tiaAgatha).
persona(mayordomo).
persona(charles).

odia(charles, UnaPersona):-
    viveEnMansion(UnaPersona),
    not(odia(tiaAgatha, UnaPersona)).

odia(tiaAgatha, UnaPersona):-
    not(UnaPersona = mayordomo).
    viveEnDreadbury(UnaPersona),

odia(mayordomo, UnaPersona):-
    odia(tiaAgatha, UnaPersona).

masRico(UnaPersona,agatha):-
    viveEnMansion(UnaPersona),
    not(odia(mayordomo,UnaPersona)).






