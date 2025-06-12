-module(listen).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12345, [{reuseaddr, true}]),
    wait_connect(ListenSocket, 0).

wait_connect(ListenSocket, N) ->
    io:fwrite("Escuchando...~n", []),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn (fun () -> wait_connect (ListenSocket, N+1) end),
    node_register(Socket).

node_register(Socket) ->
    receive
        {ok, NodeId} -> knownNodes ! {new, NodeId}
    end,
    get_request(Socket).

get_request(Socket) ->
    io:fwrite("Esperando mensajes de ~p~n", [Socket]),
    receive
        _X -> ok,
        get_request(Socket)
    end.