-module(connect).
-export([start/0]).

-import(filelib, [wildcard/1]).
-include_lib("kernel/include/file.hrl"). % para acceder a #file_info

%% Inicia el servidor TCP
start() ->
    case gen_tcp:listen(12544, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]) of
        {ok, ListenSocket} ->
            io:format("Servidor TCP escuchando en puerto 12544...~n", []),
            accept(ListenSocket);
        {error, Reason} ->
            io:format("Error al iniciar escucha TCP: ~p~n", [Reason])
    end.

%% Acepta conexiones entrantes concurrentemente
accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_connection(Socket) end),
            accept(ListenSocket);
        {error, Reason} ->
            io:format("Error en accept: ~p~n", [Reason])
    end.

%% Maneja una conexión TCP entrante
handle_connection(Socket) ->
    case inet:peername(Socket) of
        {ok, {IP, Port}} ->
            io:format("Conexión recibida de ~p:~p~n", [IP, Port]);
        _ ->
            ok
    end,
    loop(Socket).

%% Loop principal de manejo de mensajes por conexión
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Msg = binary_to_list(Data),
            io:format("Mensaje recibido: ~s~n", [Msg]),
            handle_message(Msg, Socket),
            loop(Socket);
        {error, closed} ->
            io:format("Conexión cerrada por el cliente~n", []);
        {error, Reason} ->
            io:format("Error en conexión: ~p~n", [Reason])
    end.

%% Analiza y responde a mensajes
handle_message(Msg, Socket) ->
    case string:tokens(string:trim(Msg), " \n\r") of
        ["SEARCH_REQUEST", NodeId, Pattern] ->
            io:format("Recibido SEARCH_REQUEST de ~s con patrón ~s~n", [NodeId, Pattern]),
            send_search_responses(Socket, NodeId, Pattern);
        _ ->
            io:format("Mensaje no reconocido: ~s~n", [Msg])
    end.

%% Genera y envía respuestas de búsqueda
send_search_responses(Socket, NodeId, Pattern) ->
    FullPattern = filename:join("./Compartida", Pattern),
    case wildcard(FullPattern) of
        [] ->
            io:format("No se encontraron archivos que coincidan con ~s~n", [Pattern]);
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
