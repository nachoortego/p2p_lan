-module(nodo).
-export([init/1]).

print_descargas() -> 
    case listar_archivos:listar("Descargas") of
        {error, Reason} -> 
            io:format("Error: ~s~n", [Reason]);
        Archivos -> 
            io:format("Archivos descargados: ~p~n", [Archivos])
    end.

print_compartidos() -> 
    case listar_archivos:listar("Compartida") of
        {error, Reason} -> 
            io:format("Error: ~s~n", [Reason]);
        Archivos -> 
            io:format("Archivos compartidos: ~p~n", [Archivos])
    end.

init(Id) ->
    io:format("Hola soy el nodo ~p~n", [Id]),
    print_compartidos(),
    print_descargas(),
    spawn(sv:start()).