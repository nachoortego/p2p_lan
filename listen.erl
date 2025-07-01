-module(listen).
-export([start/1]).

-define(PORT, 12346).

start(Socket) ->
    loop(Socket).

loop(Socket) ->
    MyIP = nodo:get_my_ip(),
    case gen_udp:recv(Socket, 0) of
        {ok, {Host, _Port, Data}} ->
            case Host =:= MyIP of
                true ->
                    loop(Socket);
                false ->
                    Msg = binary_to_list(Data),
                    handle_message(Msg, Host, Socket)
            end;
        {error, timeout} ->
            loop(Socket)
    end.

handle_message(Msg, Host, Socket) ->
    case string:tokens(string:trim(Msg), " \n") of
        ["HELLO", NodeId, TcpPort] ->
            knownNodes ! {new, NodeId, list_to_integer(TcpPort), Host},
            loop(Socket);

        ["NAME_REQUEST", ReqId] ->
            knownNodes ! {exists, ReqId, self()},
            receive
                {exists, ReqId} ->
                    send_invalid_name(Socket, Host, ReqId);
                {not_exists, ReqId} ->
                    getId ! {id, self()},
                    receive
                        {ok, Id} ->
                            if
                                ReqId =:= Id ->
                                    send_invalid_name(Socket, Host, ReqId);
                                true ->
                                    ok
                            end
                    end
            end,
            loop(Socket);

        _ ->
            loop(Socket)
    end.

send_invalid_name(_ListenSocket, Host, NodeId) ->
    Msg = io_lib:format("INVALID_NAME ~s\n", [NodeId]),
    {ok, SendSocket} = gen_udp:open(0, [binary, {broadcast, true}]),
    gen_udp:send(SendSocket, Host, 12346, list_to_binary(Msg)),
    gen_udp:close(SendSocket).