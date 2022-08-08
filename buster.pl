herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

cazafantasmas(egon,aspiradora(200)).
cazafantasmas(egon,trapeador).
cazafantasmas(egon,bordedadora).
cazafantasmas(peter,trapeador).
cazafantasmas(peter,varitaDeNeutrones).

tieneIntegranteHerramienta(Integrante,Herramienta):-
    cazafantasmas(Integrante,Herramienta).

tieneIntegranteHerramienta(Integrante,aspiradora(Valor)):-
    cazafantasmas(Integrante,aspiradora(ValorDeAspiradora)),
    Valor =< ValorDeAspiradora.

puedeRealizarTarea(Integrante,Tarea):-
    cazafantasmas(Integrante,_),
    requiereHerramienta(Tarea,_),
    forall(requiereHerramienta(Tarea,Herramienta),tieneIntegranteHerramienta(Integrante,Herramienta)).

puedeRealizarTarea(Integrante,_):-
    cazafantasmas(Integrante,varitaDeNeutrones).

requiereHerramienta(Tarea,Herramienta):-
    herramientasRequeridas(Tarea,Herramientas),
    member(Herramienta,Herramientas).

tareaPedida(Cliente,Tarea,Metros).

precio(Tarea,PrecioPorMetro).

cuantoSeLeCobra(Cliente,Plata):-
    tareaPedida(Cliente,Tarea,Metros),
    precio(Tarea,PrecioPorMetro),
    Plata is Metros * PrecioPorMetro.


aceptaPedidoCliente(Integrante,Cliente):-
    puedeHacerTodasTareas(Integrante,Cliente),
    dispuesto(Integrante,Cliente).


puedeHacerTodasTareas(Integrante,Cliente):-
    cazafantasmas(Integrante,_),
    tareaPedida(Cliente,_,_),
    forall(tareaPedida(Cliente,Tarea,_),puedeRealizarTarea(Integrante,Tarea)).

dispuesto(ray,Cliente):-
    not(tareaPedida(Cliente, limpiarPasto,_)).

dispuesto(winston,Cliente):-
    cuantoSeLeCobra(Cliente,Pago),
    Pago >= 500.

dispuesto(egon,Cliente):-
    not(tareaCompleja(Cliente)).

tareaCompleja(Cliente):-
    tareaPedida(Cliente,Tarea,_),
    herramienta(Tarea,UnaHerramienta),
    herramienta(Tarea,OtraHerramienta),
    UnaHerramienta \= OtraHerramienta.

herramienta(Tarea,Herarmienta):-
    herramientasRequeridas(Tarea,Herramientas),
    member(Herramienta,Herramientas).


dispuesto(peter,_).