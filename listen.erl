-module(listen).
-export([start/1]).

%% Inicia el loop
start(Socket) ->
    loop(Socket).

%% Filtra los mensajes UDP que no son de sí mismo y los handlea
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

%% Handlea los distintos mensajes UDP
handle_message(Msg, Host, Socket) ->
    case string:tokens(string:trim(Msg), " \n") of
        %% Si recibe HELLO, agrega o actualiza al map de nodos conocidos
        ["HELLO", NodeId, TcpPort] ->
            knownNodes ! {new, NodeId, list_to_integer(TcpPort), Host},
            loop(Socket);
        %% Si recibe NAME_REQUEST, chequea que no exista en nodos conocidos y que sea distinto a su propio id
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

%% Envia un INVALID_NAME <nodo_id> al nodo con un name request incorrecto por unicast
send_invalid_name(Socket, Host, NodeId) ->
    Msg = io_lib:format("INVALID_NAME ~s\n", [NodeId]),
    io:format("Enviando mensaje: ~s a ~p~n", [Msg, Host]),
    gen_udp:send(Socket, Host, 12346, iolist_to_binary(Msg)).
