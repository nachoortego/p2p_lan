-module(connect).
-export([start/0]).

-import(filelib, [wildcard/1]).
-include_lib("kernel/include/file.hrl").

start() ->
    case gen_tcp:listen(12544, [
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

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_connection_loop(Socket) end),
            accept(ListenSocket);
        {error, Reason} ->
            io:format("Error en accept: ~p~n", [Reason])
    end.

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

handle_message(Msg, Socket) ->
    case string:tokens(string:trim(Msg), " \n\r") of
        ["SEARCH_REQUEST", NodeId, Pattern] ->
            send_search_responses(Socket, NodeId, Pattern);
        ["DOWNLOAD_REQUEST", Filename] ->
            send_file:send_file(Socket, Filename);
        _ ->
            io:format("Mensaje no reconocido: ~s~n", [Msg])
    end.

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
