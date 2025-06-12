-module(cli).
-export([cli/0, print_descargas/0, print_compartidos/0]).

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

cli() ->
    case io:get_line("> ") of
        "id_nodo\n" ->
            getId ! {id, self()},
            receive
                {ok, Id} -> io:format("ID del nodo: ~p~n", [Id])
            end,
            cli();
        "listar_descargas\n" ->
            print_descargas(),
            cli();
        "listar_compartidosos\n" ->
            print_compartidos(),
            cli();
        "salir\n" ->
            io:format("Saliendo...~n"),
            ok;
        "help\n" ->
            io:format("Comandos disponibles:~n"),
            io:format("  id_nodo - Muestra el ID del nodo~n"),
            io:format("  listar_descargas - Lista los archivos descargados~n"),
            io:format("  listar_compartidosos - Lista los archivos compartidos~n"),
            io:format("  salir - Salir del programa~n"),
            cli();
        _ ->
            io:format("Comando no reconocido.~n"),
            cli()
    end.