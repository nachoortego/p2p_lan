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
        "listar_compartidos\n" ->
            print_compartidos(),
            cli();
        "nodos_conocidos\n" ->
            case known_nodes:get() of
                {ok, NodeMap} -> 
                    io:format("Nodos conocidos: ~p~n", [NodeMap]),
                    cli();
                {error, Reason} -> 
                    io:format("Error al obtener nodos conocidos: ~s~n", [Reason]),
                    cli()
            end;
        "pedir_archivo\n" ->
            FileName = string:trim(io:get_line("Nombre del archivo: ")),
            NodeId = string:trim(io:get_line("Node ID: ")),
            Message = io_lib:format("SEARCH_REQUEST ~s ~s~n", [NodeId, FileName]),
            udp_broadcast:send(list_to_binary(Message)),
            cli();
        "salir\n" ->
            io:format("Saliendo...~n"),
            ok;
        "help\n" ->
            io:format("Comandos disponibles:~n"),
            io:format("  id_nodo - Muestra el ID del nodo~n"),
            io:format("  listar_descargas - Lista los archivos descargados~n"),
            io:format("  listar_compartidos - Lista los archivos compartidos~n"),
            io:format("  salir - Salir del programa~n"),
            cli();
        _ ->
            io:format("Comando no reconocido.~n"),
            cli()
    end.