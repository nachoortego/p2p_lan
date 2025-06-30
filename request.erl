-module(request).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12345, [{reuseaddr, true}]),
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
            % Aquí puedes procesar el mensaje recibido
            % Por ejemplo, responder al cliente:
            gen_tcp:send(Socket, <<"Mensaje recibido">>),
            loop(Socket);
        {error, closed} ->
            io:fwrite("Conexión cerrada~n", []);
        {error, Reason} ->
            io:fwrite("Error en la conexión: ~p~n", [Reason])
    end.

get_request(Socket) ->
    io:fwrite("Esperando mensajes de ~p~n", [Socket]),
    receive
        -> ok,
        get_request(Socket)
    end.


handle_message(Msg, Host, Socket) ->
    case string:tokens(string:trim(Msg), " \n") of
        ["HELLO", NodeId, TcpPort] ->
            knownNodes ! {new, NodeId, list_to_integer(TcpPort), Host},
            io:format("HELLO de ~s (~p) TCP:~s~n", [NodeId, Host, TcpPort]),
            loop(Socket);

        ["NAME_REQUEST", ReqId] ->
            knownNodes ! {exists, ReqId, self()},
            receive
                {exists, ReqId} ->
                    io:format("El id ~s ya está en uso, enviando INVALID_NAME~n", [ReqId]),
                    send_invalid_name(Socket, Host, ReqId);
                {not_exists, ReqId} ->
                    io:format("El id ~s no está en uso, enviando HELLO~n", [ReqId]),
                    send_hello(Socket, ReqId, ?PORT)
            end,
            loop(Socket);

        _ ->
            loop(Socket)
    end.