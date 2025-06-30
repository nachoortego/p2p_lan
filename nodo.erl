-module(nodo).
-export([init/0]).

broadcast(Socket, Message) ->
    Address = {255, 255, 255, 255}, 
    gen_udp:send(Socket, Address, 12346, Message).

generate_id(Socket) ->
    Id = lists:map(
            fun(_) ->
                lists:nth(rand:uniform(62),
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
            end,
            lists:seq(1, 4)),
    % Id = "Hola",
    
    Msg = "NAME_REQUEST " ++ Id ++ "\n",
    io:format("Broadcast: ~p~n", [Msg]),

    gen_udp:send(Socket, {255, 255, 255, 255}, 12346, Msg),

    StartTime = erlang:monotonic_time(millisecond),
    Result = wait_response(Socket, Id, StartTime),

    gen_udp:close(Socket),
    Result.

wait_response(Socket, Id, StartTime) ->
    Timeout = 3000,
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    Remaining = Timeout - Elapsed,
    if
        Remaining =< 0 ->
            io:format("Tiempo agotado, usando ID: ~p~n", [Id]),
            Id;
        true ->
            receive
                {udp, Socket, _IP, _Port, Binary} ->
                    Str = binary_to_list(Binary),
                    % io:format("Mensaje UDP crudo: ~p~n", [Str]),
                    Tokens = string:tokens(string:trim(Str), " "),
                        case Tokens of
                            ["INVALID_NAME", Id] ->
                                io:format("Nombre inválido detectado: ~p~n", [Id]),
                                timer:sleep(5000),
                                generate_id(Socket); 
                            Token ->
                                io:format("Mensaje recibido: ~p~n", [Token]),
                                wait_response(Socket, Id, StartTime)
                        end
            after Remaining ->
                io:format("No se recibió INVALID_NAME, usando ID: ~p~n", [Id]),
                Id
            end
    end.

hello_loop(Socket, MyId) ->
    Msg = "HELLO " ++ MyId ++ " 12544\n",
    io:format("Broadcast: ~p~n", [Msg]),
    gen_udp:send(Socket, {255, 255, 255, 255}, 12346, Msg),
    timer:sleep(25000),
    hello_loop(Socket, MyId).

get_id(Id) ->
    receive
        {id, Pid} -> Pid ! {ok, Id}
    end,
    get_id(Id).

% {
%     "idNodo": {
%         "ip": "123.123.123",
%         "puerto": "12345"
%     },
% }
known_nodes(NodeMap) ->
    receive
        {new, NodeId, Port, NodeIP} ->
            case maps:is_key(NodeId, NodeMap) of
                true -> known_nodes(NodeMap);
                false ->
                    NodeInfo = #{
                        ip => NodeIP,
                        puerto => Port
                    },
                    known_nodes(maps:put(NodeId, NodeInfo, NodeMap))
            end;
        {get, From} ->
            From ! {ok, NodeMap},
            known_nodes(NodeMap);
        {exists, NodeId, From} ->
            case maps:is_key(NodeId, NodeMap) of
                true -> From ! {exists, NodeId};
                false -> From ! {not_exists, NodeId}
            end,
            known_nodes(NodeMap)
    end.

init() ->
    {ok, Socket} = gen_udp:open(12346, [binary, {broadcast, true}, {reuseaddr, true}, {active, true}]),
    MyId = generate_id(Socket),
    
    GetIdPid = spawn(fun() -> get_id(MyId) end),
    register(getId, GetIdPid),
    
    getId ! {id, self()},
    
    receive
        {ok, Id123} ->
            io:format("Hola soy el nodo ~p~n", [Id123])
    end,

    spawn(fun() -> listen:start(Socket) end),
    spawn(fun() -> hello_loop(Socket, MyId) end),
    
    KnownNodesPid = spawn(fun() -> known_nodes(#{}) end),
    register(knownNodes, KnownNodesPid),

    cli:cli().
