-module(connect).
-export([start/0]).

-import(filelib, [wildcard/1]).
-include_lib("kernel/include/file.hrl").

%% Inicializa la escucha en el puerto 12544 por TCP
start() ->
    case gen_tcp:listen(12345, [
    binary,
    {reuseaddr, true},
    {active, false},
    {ip, {0,0,0,0}}  % importante
]) of
        {ok, ListenSocket} ->
            accept(ListenSocket);
        {error, Reason} ->
            io:format("Error al iniciar escucha TCP: ~p~n", [Reason])
    end.

%% Acepta conexiones entrantes y las handlea
accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_connection_loop(Socket) end), %% Spawnea proceso para handlear la conexion
            accept(ListenSocket); %% Sigue aceptando nuevas conexiones
        {error, Reason} ->
            io:format("Error en accept: ~p~n", [Reason])
    end.

%% Handlea las conexiones válidas, sino las rechaza
handle_connection_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Msg = binary_to_list(Data),
            handle_message(Msg, Socket),
            handle_connection_loop(Socket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("Error en conexión: ~p~n", [Reason])
    end.

%% Handlea las SEARCH_REQUEST y DOWNLOAD_REQUEST
handle_message(Msg, Socket) ->
    case string:tokens(string:trim(Msg), " \n\r") of
        %% Envia la respuesta al nodo que hizo SEARCH_REQUEST
        ["SEARCH_REQUEST", NodeId, Pattern] ->
            send_search_responses(Socket, NodeId, Pattern);
        %% Envia el archivo <Filename> al nodo que hizo DOWNLOAD_REQUEST
        ["DOWNLOAD_REQUEST", Filename] ->
            send_file:send_file(Socket, Filename);
        _ ->
            io:format("Mensaje no reconocido: ~s~n", [Msg])
    end.

%% Matchea con wildcards los archivos que coinciden con la SEARCH_REQUEST y los envía
send_search_responses(Socket, NodeId, Pattern) ->
    FullPattern = filename:join("./Compartida", Pattern),
    case wildcard(FullPattern) of
        %% No hay coincidencias
        [] ->
            io:format("No se encontraron archivos que coincidan con ~s~n", [Pattern]);
        %% Recorre la lista de archivos y los envía por TCP al nodo que hizo la SEARCH_REQUEST copn su tamaño y nombre
        Matches ->
            lists:foreach(
                fun(Filename) ->
                    case file:read_file_info(Filename) of
                        {ok, FileInfo} ->
                            Size = FileInfo#file_info.size,
                            BaseName = filename:basename(Filename),
                            Response = io_lib:format("SEARCH_RESPONSE ~s ~s ~p~n", [NodeId, BaseName, Size]),
                            gen_tcp:send(Socket, lists:flatten(Response));
                        {error, Reason} ->
                            io:format("Error leyendo info de ~s: ~p~n", [Filename, Reason])
                    end
                end,
                Matches
            )
    end.
