-module(listen).
-export([start/0]).

-define(PORT, 12346).

start() ->
    {ok, Socket} = gen_udp:open(?PORT, [binary, {reuseaddr, true}, {broadcast, true}, {active, true}]),
    io:format("Escuchando UDP en todas las IPs, puerto ~p...~n", [?PORT]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, _Port, Data} ->
            Msg = binary_to_list(Data),
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
                    io:format("El id ~s no está en uso, enviando HELLO~n", [ReqId]),
                    send_hello(Socket, ReqId, ?PORT)
            end,
            loop(Socket);

        _ ->
            loop(Socket)
    end.

send_hello(Socket, NodeId, TcpPort) ->
    Msg = io_lib:format("HELLO ~s ~p\n", [NodeId, TcpPort]),
    gen_udp:send(Socket, {255,255,255,255}, ?PORT, list_to_binary(Msg)).

% send_name_request(Socket, NodeId) ->
%     Msg = io_lib:format("NAME_REQUEST ~s\n", [NodeId]),
%     gen_udp:send(Socket, {255,255,255,255}, ?PORT, list_to_binary(Msg)).

send_invalid_name(Socket, Host, NodeId) ->
    Msg = io_lib:format("INVALID_NAME ~s\n", [NodeId]),
    gen_udp:send(Socket, Host, ?PORT, list_to_binary(Msg)).