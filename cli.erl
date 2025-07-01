-module(cli).
-export([cli/0, print_descargas/0, print_compartidos/0]).

%% Imprime por consola la lista de archivos de la carpeta Descargas
print_descargas() -> 
    case listar_archivos:listar("Descargas") of
        {error, Reason} -> 
            io:format("Error al listar descargas: ~s~n", [Reason]);
        [] ->
            io:format("No hay archivos descargados.~n");
        Archivos -> 
            io:format("Archivos descargados:~n"),
            lists:foreach(
                fun(Archivo) -> io:format("  - ~s~n", [Archivo]) end,
                Archivos
            )
    end.

%% Imprime por consola la lista de archivos de la carpeta Compartida
print_compartidos() -> 
    case listar_archivos:listar("Compartida") of
        {error, Reason} -> 
            io:format("Error al listar compartidos: ~s~n", [Reason]);
        [] ->
            io:format("No hay archivos compartidos.~n");
        Archivos -> 
            io:format("Archivos compartidos:~n"),
            lists:foreach(
                fun(Archivo) -> io:format("  - ~s~n", [Archivo]) end,
                Archivos)
    end.

%% Recibe comandos por entrada estándar y devuelve el resultado
cli() ->
    case io:get_line("> ") of
        %% Devuelve Id del nodo
        "id_nodo\n" ->
            getId ! {id, self()},
            receive
                {ok, Id} -> io:format("ID del nodo: ~p~n", [Id])
            end,
            cli();
        %% Imprime los archivos de la carpeta Descargas 
        "listar_descargas\n" ->
            print_descargas(),
            cli();
        %% Descarga un archivo
        "descargar\n" ->
            FileName = string:trim(io:get_line("Nombre del archivo a descargar: ")),
            NodeId = string:trim(io:get_line("Node ID del nodo origen: ")),
            knownNodes ! {get, self()},
            receive
                {ok, NodeMap} ->
                    case maps:is_key(NodeId, NodeMap) of
                        false ->
                            io:format("Node ID ~s no encontrado en nodos conocidos.~n", [NodeId]),
                            cli();
                        true ->
                            Nodo = maps:get(NodeId, NodeMap),
                            file_download:ask_for_file(FileName, {maps:get(ip, Nodo), maps:get(puerto, Nodo)})
                    end
            end,
            cli();
        %% Imprime los archivos de la carpeta Compartida 
        "listar_compartidos\n" ->
            print_compartidos(),
            cli();
        %% Imprime el map de nodos conocidos
        "nodos_conocidos\n" ->
            knownNodes ! {get, self()},
            receive
            {ok, NodeMap} -> 
                io:format("Nodos conocidos:~n"),
                maps:fold(
                    fun(NodeId, Info, _) ->
                        io:format("  ID: ~s~n    IP: ~p~n    Puerto: ~p~n", [
                            NodeId,
                            maps:get(ip, Info),
                            maps:get(puerto, Info)
                        ])
                    end,    
                    ok,
                    NodeMap
                ),
                cli();
            {error, Reason} -> 
                io:format("Error al obtener nodos conocidos: ~s~n", [Reason]),
                cli()
            end;
        %% Busca archivos en la red
        "buscar\n" ->
            Pattern = string:trim(io:get_line("Patrón de archivo: ")),
            buscar_archivos(Pattern),
            cli();
        %% Desconecta el nodo de la red
        "salir\n" ->
            io:format("Saliendo...~n"),
            ok;
        %% Imprime la lista de comandos válidos
        "help\n" ->
            io:format("Comandos disponibles:~n"),
            io:format("  id_nodo - Muestra el ID del nodo~n"),
            io:format("  listar_descargas - Lista los archivos descargados~n"),
            io:format("  listar_compartidos - Lista los archivos compartidos~n"),
            io:format("  salir - Salir del programa~n"),
            io:format("  buscar - Buscar archivos en la red~n"),
            io:format("  descargar - Descargar archivo de algún nodo~n"),
            io:format("  nodos_conocidos - Muestra los nodos conocidos (ID, IP y puerto)~n"),
            io:format("  salir - Salir del programa~n"),
            cli();
        _ ->
            io:format("Comando no reconocido.~n"),
            cli()
    end.

%% Envía SEARCH_REQUEST con el patrón dado a cada nodo conocido de la red
buscar_archivos(Pattern) ->
    getId ! {id, self()},
    receive 
        {ok, MyiId} -> ok 
    end,

    knownNodes ! {get, self()},
    receive
        {ok, NodeMap} ->
            lists:foreach(
                fun({NodeId, InfoMap}) ->
                    Ip = maps:get(ip, InfoMap),
                    Port = maps:get(puerto, InfoMap),
                    spawn(fun() -> consulta_tcp(NodeId, Ip, Port, MyiId, Pattern) end) %% Spawnea un proceso por cada consulta
                end,
                maps:to_list(NodeMap)
            );

        _ -> io:format("Error al obtener nodos conocidos~n")
    end.

%% Envía una SEARCH_REQUEST por TCP a un nodo dado
consulta_tcp(RemoteNodeId, Ip, Port, MyiId, Pattern) ->
    io:format("DEBUG - Id: ~p~n", [MyiId]),
    io:format("DEBUG - Pattern: ~p~n", [Pattern]),
    case gen_tcp:connect(Ip, Port, [binary, {active, false}]) of
        {ok, Socket} ->
            try
                MyIdBin = list_to_binary(MyiId),
                PatternBin = list_to_binary(Pattern),
                MsgBin = <<"SEARCH_REQUEST ", MyIdBin/binary, " ", PatternBin/binary, "\n">>,
                gen_tcp:send(Socket, MsgBin),
                recibir_respuestas(Socket, RemoteNodeId)
            catch
                error:badarg ->
                    io:format("❌ ERROR al convertir a binario: ~p ~p~n", [MyiId, Pattern])
            end;
        {error, Reason} ->
            io:format("No se pudo conectar con ~s (~p): ~p~n", [RemoteNodeId, Ip, Reason])
    end.

%% Recibe por TCP la respuesta a una SEARCH_REQUEST
recibir_respuestas(Socket, NodeId) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Bin} ->
            Lineas = string:split(binary_to_list(Bin), "\n", all),
            lists:foreach(fun(Line) -> imprimir_respuesta(NodeId, Line) end, Lineas), %% Recorre la info recibida e imprime por consola
            recibir_respuestas(Socket, NodeId);
        {error, timeout} -> ok;
        {error, closed} -> ok;
        {error, _} -> ok
    end.

%% Imprime las respuestas válidas por consola
imprimir_respuesta(_NodeId, "") -> ok;
imprimir_respuesta(NodeId, Line) ->
    case string:tokens(Line, " ") of
        ["SEARCH_RESPONSE", _, Nombre, TamañoStr] ->
            io:format("~s: ~s (~s bytes)~n", [NodeId, Nombre, TamañoStr]);
        Line -> io:format("Respuesta no reconocida: ~s~n", [Line])
    end.