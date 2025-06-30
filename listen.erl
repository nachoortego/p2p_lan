-module(listen).
-export([start/1]).

-define(PORT, 12346).

start(Socket) ->
    io:format("Escuchando UDP en todas las IPs, puerto ~p...~n", [12346]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, _Port, Data} ->
            Msg = binary_to_list(Data),
            io:format("Mensaje recibido de ~p: ~s~n", [Host, Msg]),
            handle_message(Msg, Host, Socket)
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
                    getId ! {id, self()},
                    receive
                        {ok, Id} ->
                            if
                                ReqId =:= Id ->
                                    io:format("El id solicitado (~s) es mi propio id, enviando INVALID_NAME~n", [ReqId]),
                                    send_invalid_name(Socket, Host, ReqId);
                                true ->
                                    io:format("El id solicitado (~s) no es mi propio id ni esta en la lista de nodos conocidos, no hago nada~n", [ReqId])
                            end
                    end
            end,
            loop(Socket);

        Token ->
            io:format("Mensaje no reconocido: ~s~n", [Token]),
            loop(Socket)
    end.

% send_hello(Socket, NodeId, TcpPort) ->
%     Msg = io_lib:format("HELLO ~s ~p\n", [NodeId, TcpPort]),
%     gen_udp:send(Socket, {255,255,255,255}, ?PORT, list_to_binary(Msg)).

% send_name_request(Socket, NodeId) ->
%     Msg = io_lib:format("NAME_REQUEST ~s\n", [NodeId]),
%     gen_udp:send(Socket, {255,255,255,255}, ?PORT, list_to_binary(Msg)).

send_invalid_name(_ListenSocket, Host, NodeId) ->
    io:format("Respondemos a IP: ~p~n", [Host]),
    Msg = io_lib:format("INVALID_NAME ~s\n", [NodeId]),
    {ok, SendSocket} = gen_udp:open(0, [binary, {broadcast, true}]),
    gen_udp:send(SendSocket, Host, 12346, list_to_binary(Msg)),
    gen_udp:close(SendSocket).