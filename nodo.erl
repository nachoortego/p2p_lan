-module(nodo).
-export([init/0, get_my_ip/0]).

get_my_ip() ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:connect(Socket, {192,168,0,1}, 12345),
    {ok, {MyIP, _}} = inet:sockname(Socket),
    gen_udp:close(Socket),
    MyIP.

generate_id(Socket) ->
    Id = lists:map(
            fun(_) ->
                lists:nth(rand:uniform(62),
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
            end,
            lists:seq(1, 4)),
    % Id = "asda",

    Msg = "NAME_REQUEST " ++ Id ++ "\n",
    gen_udp:send(Socket, {192, 168, 0, 255}, 12346, Msg),

    StartTime = erlang:monotonic_time(millisecond),
    Result = wait_response(Socket, Id, StartTime),

    Result.

wait_response(Socket, Id, StartTime) ->
    Timeout = 10000,
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    Remaining = Timeout - Elapsed,
    if
        Remaining =< 0 ->
            Id;
        true ->
            MyIP = get_my_ip(),
            case gen_udp:recv(Socket, 0, Remaining) of
                {ok, {IP, _Port, Binary}} ->
                    case IP =:= MyIP of
                        true -> 
                            wait_response(Socket, Id, StartTime); % Ignora propio mensaje
                        false -> 
                            Str = binary_to_list(Binary),
                            Tokens = string:tokens(string:trim(Str), " "),
                            case Tokens of
                                ["INVALID_NAME", Id] ->
                                    timer:sleep(1000),
                                    generate_id(Socket);
                                ["NAME_REQUEST", Id] ->
                                    listen:send_invalid_name(Socket, IP, Id),
                                _ ->
                                    wait_response(Socket, Id, StartTime)
                            end
                    end;

                {error, timeout} -> 
                    Id
            end
    end.

hello_loop(Socket, MyId) ->
    Msg = "HELLO " ++ MyId ++ " 12544\n",
    gen_udp:send(Socket, {192, 168, 0, 255}, 12346, Msg),
    timer:sleep(15000),
    hello_loop(Socket, MyId).

get_id(Id) ->
    receive
        {id, Pid} -> Pid ! {ok, Id}
    end,
    get_id(Id).

check_nodes() ->
    timer:sleep(10000),
    knownNodes ! {checkNodes},
    check_nodes().

known_nodes(NodeMap) ->
    receive
        {new, NodeId, Port, NodeIP} ->
            getId ! {id, self()},
            receive
                {ok, NodeId} -> known_nodes(NodeMap);
                {ok, _} ->
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
                    end
            end;
        {checkNodes} ->
            Now = erlang:monotonic_time(millisecond),
            Time = 45000,
            FilteredMap = maps:filter(
                fun(_Key, NodeInfo) ->
                    Now - maps:get(last_update, NodeInfo) =< Time
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
            known_nodes(NodeMap);
        _ ->
            io:format("Mensaje desconocido en known_nodes: ~p~n", [NodeMap]),
            known_nodes(NodeMap)
    end.

init() ->
    {ok, Socket} = gen_udp:open(12346, [binary, {broadcast, true}, {reuseaddr, true}, {active, false}]),
    MyId = generate_id(Socket),

    
    GetIdPid = spawn(fun() -> get_id(MyId) end),
    register(getId, GetIdPid),
    
    KnownNodesPid = spawn(fun() -> known_nodes(#{}) end),
    register(knownNodes, KnownNodesPid),

    getId ! {id, self()},
    
    receive
        {ok, Id123} ->
            io:format("Hola soy el nodo ~p~n", [Id123])
    end,


    timer:sleep(1000),
    spawn(fun() -> listen:start(Socket) end), % UDP
    spawn(fun() -> connect:start() end), %TCP
    spawn(fun() -> hello_loop(Socket, MyId) end),
    spawn(fun () -> check_nodes() end),

    % gen_udp:close(Socket),

    cli:cli().
