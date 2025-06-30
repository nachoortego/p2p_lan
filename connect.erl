-module(connect).
-export([start/0]).
-import(filelib, [wildcard/2]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12544, [{reuseaddr, true}]),
    wait_connect(ListenSocket, 0).

wait_connect(ListenSocket, N) ->
    io:fwrite("Escuchando...~n", []),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn (fun () -> wait_connect (ListenSocket, N+1) end),
    handle_connection(Socket).

handle_connection(Socket) ->
    io:fwrite("Nueva conexión de ~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:fwrite("Mensaje recibido: ~s~n", [Data]),
            handle_message(Data, inet:peername(Socket), Socket),
            loop(Socket);
        {error, closed} ->
            io:fwrite("Conexión cerrada~n", []);
        {error, Reason} ->
            io:fwrite("Error en la conexión: ~p~n", [Reason])
    end.

handle_message(Msg, _Host, Socket) ->
    case string:tokens(string:trim(Msg), " \n") of
        ["SEARCH_REQUEST", NodeId, Pattern] ->
            io:format("SEARCH_REQUEST de ~s: ~s~n", [NodeId, Pattern]),
            send_search_responses(Socket, NodeId, Pattern),
            loop(Socket);

        _ ->
            loop(Socket)
    end.

-include_lib("kernel/include/file.hrl").

send_search_responses(Socket, NodeId, Pattern) ->
    Matches = wildcard(Pattern, [filename:join("./Compartida", "*")]),
    lists:foreach(
        fun(Filename) ->
            {ok, FileInfo} = file:read_file_info(Filename),
            Size = FileInfo#file_info.size,
            BaseName = filename:basename(Filename),
            Response = io_lib:format("SEARCH_RESPONSE ~s ~s ~p~n", [NodeId, BaseName, Size]),
            gen_tcp:send(Socket, lists:flatten(Response))
        end,
        Matches
    ).
