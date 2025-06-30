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
            knownNodes ! {get, self()},
            receive
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
        "buscar\n" ->
            Patron = string:trim(io:get_line("Patrón de archivo: ")),
            buscar_archivos(Patron),
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

buscar_archivos(Patron) ->
    %% Archivos locales
    case listar_archivos:listar("Compartida") of
        {ok, Archs} ->
            io:format("Coincidencias locales: ~p~n", [filtrar(Patron, Archs)]);
        _ -> ok
    end,

    %% Consultar nodos conocidos
    getId ! {id, self()},
    receive {ok, MiId} -> ok end,

    knownNodes ! {get, self()},
    receive
        {ok, NodeMap} ->
            lists:foreach(
                fun({NodeId, InfoMap}) ->
                    Ip = maps:get(ip, InfoMap),
                    Port = maps:get(puerto, InfoMap),
                    spawn(fun() -> consulta_tcp(NodeId, Ip, Port, MiId, Patron) end)
                end,
                maps:to_list(NodeMap)
            );

        _ -> io:format("Error al obtener nodos conocidos~n")
    end.

filtrar(Patron, Archs) ->
    %% Solo devuelve los archivos que matchean el patrón (wildcard)
    Pattern = filename:join(".", Patron),
    [A || A <- Archs, filelib:wildcard_match(Pattern, A)].

consulta_tcp(RemoteNodeId, Ip, Port, MiId, Patron) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}]) of
        {ok, Socket} ->
            Msg = io_lib:format("SEARCH_REQUEST ~s ~s~n", [MiId, Patron]),
            gen_tcp:send(Socket, lists:flatten(Msg)),
            recibir_respuestas(Socket, RemoteNodeId),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("No se pudo conectar con ~s (~p): ~p~n", [RemoteNodeId, Ip, Reason])
    end.

recibir_respuestas(Socket, NodeId) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Bin} ->
            Lineas = string:split(binary_to_list(Bin), "\n", all),
            lists:foreach(fun(Line) -> imprimir_respuesta(NodeId, Line) end, Lineas),
            recibir_respuestas(Socket, NodeId); % sigue leyendo
        {error, timeout} -> ok;
        {error, closed} -> ok;
        {error, _} -> ok
    end.

imprimir_respuesta(_NodeId, "") -> io:format("Respuesta vacía recibida~n");
imprimir_respuesta(NodeId, Line) ->
    case string:tokens(Line, " ") of
        ["SEARCH_RESPONSE", _, Nombre, TamañoStr] ->
            io:format("~s: ~s (~s bytes)~n", [NodeId, Nombre, TamañoStr]);
        Line -> io:format("Respuesta no reconocida: ~s~n", [Line])
    end.