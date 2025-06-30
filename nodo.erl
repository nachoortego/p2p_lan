-module(nodo).
-export([init/0, get_my_ip/0]).

% broadcast(Socket, Message) ->
%     Address = {255, 255, 255, 255}, 
%     gen_udp:send(Socket, Address, 12346, Message).

get_my_ip() ->
    {ok, IFs} = inet:getif(),
    case lists:filter(fun({{_,_,_,_}, B, _C}) -> B =/= 127 end, IFs) of
        [{{A,B,C,D}, _, _}|_] -> {A,B,C,D};
        _ -> {127,0,0,1}
    end.

generate_id(Socket) ->
    Id = lists:map(
            fun(_) ->
                lists:nth(rand:uniform(62),
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
            end,
            lists:seq(1, 4)),
    % Id = "pG7T",
    
    Msg = "NAME_REQUEST " ++ Id ++ "\n",
    % io:format("Broadcast: ~p~n", [Msg]),

    gen_udp:send(Socket, {255, 255, 255, 255}, 12346, Msg),

    StartTime = erlang:monotonic_time(millisecond),
    Result = wait_response(Socket, Id, StartTime),

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
            MyIP = get_my_ip(),
            case gen_udp:recv(Socket, 0, Remaining) of
                {ok, {IP, _Port, Binary}} ->
                    % Asegúrate de que no estamos procesando un mensaje de nuestra propia IP
                    case IP =:= MyIP of
                        true -> 
                            io:format("Ignorando mensaje de mi propia IP: ~p~n", [IP]),
                            wait_response(Socket, Id, StartTime); % No hacer nada, vuelve a esperar

                        false -> 
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
                    end;

                {error, timeout} -> 
                    io:format("Tiempo de espera agotado, usando ID: ~p~n", [Id]),
                    Id
            end
    end.


hello_loop(Socket, MyId) ->
    Msg = "HELLO " ++ MyId ++ " 12544\n",
    % io:format("Broadcast: ~p~n", [Msg]),
    gen_udp:send(Socket, {255, 255, 255, 255}, 12346, Msg),
    timer:sleep(3000),
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

check_nodes() ->
    timer:sleep(10000),
    knownNodes ! {checkNodes},
    check_nodes().

known_nodes(NodeMap) ->
    receive
        {new, NodeId, Port, NodeIP} ->
            case maps:is_key(NodeId, NodeMap) of
                true ->
                    NodeInfo = maps:get(NodeId, NodeMap),
                    UpdatedNodeInfo = maps:put(last_update, erlang:monotonic_time(millisecond), NodeInfo),
                    known_nodes(maps:put(NodeId, UpdatedNodeInfo, NodeMap));
                false ->
                    NodeInfo = #{
                        ip => NodeIP,
                        puerto => Port,
                        last_update => erlang:monotonic_time(millisecond)
                    },
                    known_nodes(maps:put(NodeId, NodeInfo, NodeMap))
            end;
        {checkNodes} ->
            Now = erlang:monotonic_time(millisecond),
            TenSeconds = 10000,
            FilteredMap = maps:filter(
                fun(_Key, NodeInfo) ->
                    Now - maps:get(last_update, NodeInfo) =< TenSeconds
                end,
                NodeMap
            ),
            known_nodes(FilteredMap);
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
    {ok, Socket} = gen_udp:open(12346, [binary, {broadcast, true}, {reuseaddr, true}, {active, false}]),
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
    spawn(fun () -> check_nodes() end),

    % gen_udp:close(Socket),

    cli:cli().
